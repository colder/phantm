package phantm.types

import phantm.phases.PhasesContext
import phantm.util._
import phantm.ast.{Trees => AST}
import phantm.cfg.ControlFlowGraph
import phantm.cfg.Trees._
import phantm.symbols._
import phantm.Settings
import phantm.annotations.AnnotationsStore
import phantm.dataflow.TransferFunction

case class TypeTransferFunction(silent: Boolean,
                                ctx: PhasesContext,
                                collectAnnotations: Boolean,
                                collectGlobals: Boolean = false,
                                inlined: Boolean = false,
                                noticesFct: (String, Positional) => Unit = Reporter.notice(_: String, _: Positional),
                                errorsFct: (String, Positional) => Unit = Reporter.error(_: String, _: Positional)) extends TransferFunction[TypeEnvironment, Statement] {
    def notice(msg: String, pos: Positional) = if (!silent) noticesFct(msg, pos)
    def error(msg: String, pos: Positional) = if (!silent) errorsFct(msg, pos)

    val trueTypes  = TNumeric union TAnyArray union TString union TTrue union TResource union TAnyObject
    val falseTypes = TNumeric union TAnyArray union TString union TFalse union TNull union TUninitialized

    val allTypes = Set[Type]() + TInt + TFloat +
                   TAnyArray + TResource + TString +
                   TAnyObject + TTrue + TFalse + TNull;

    def allTypesBut(t: Type*): Type = {
        (allTypes -- t).reduceLeft(_ union _)
    }


    def possiblyUninit(t: Type): Boolean = t match {
        case TTop =>
            true
        case TUninitialized =>
            false
        case tu: TUnion =>
            tu.types contains TUninitialized
        case _ =>
            false;
    }

    def removeUninit(removeInArrays: Boolean)(t: Type): Type = t match {
        case TTop =>
            TAny
        case TUninitialized =>
            TBottom
        case tu: TUnion =>
            tu.types.map { removeUninit(removeInArrays) } reduceLeft (_ union _)
        case ta: TArray =>
            if (removeInArrays) {
                new TArray(Map[ArrayKey, Type]() ++ ta.entries.map{ e => (e._1, removeUninit(removeInArrays)(e._2)) }, 
                           removeUninit(removeInArrays)(ta.globalInt),
                           removeUninit(removeInArrays)(ta.globalString))
            } else {
                t
            }
        case _ =>
            t
    }

    def uninitToNull(t: Type): Type = t match {
        case TTop =>
            TAny
        case TUninitialized =>
            TNull
        case tu: TUnion =>
            tu.types.map { uninitToNull } reduceLeft (_ union _)
        case _ =>
            t
    }

    def apply(node : Statement, envInit : TypeEnvironment) : TypeEnvironment = {
        var env = envInit

        def leq(t1: Type, t2: Type) = TypeLattice.leq(env, t1, t2)
        def meet(t1: Type, t2: Type) = {
            val (nenv, t) = TypeLattice.meet(env, t1, t2)
            env = nenv;
            t
        }

        def typeFromSV(sv: SimpleValue): Type = sv match {
            case PHPLong(value)      => TIntLit(value)
            case PHPFloat(value)     => TFloatLit(value)
            case PHPString(value)    => TStringLit(value)
            case PHPTrue()           => TTrue
            case PHPFalse()          => TFalse
            case PHPAny()            => TAny
            case NoVar()             => TBottom
            case PHPNull()           => TNull
            case PHPThis()           => new TObjectRef(ObjectId(-1, ObjectIdUse))
            case PHPEmptyArray()     => new TArray()
            case Instanceof(lhs, cl) => TBoolean
            case ArrayNext(ar)       => typeFromSV(ar)
            case ArrayCurElement(id: SimpleVariable) =>
                env.lookup(id) match {
                    case Some(TAnyArray) =>
                        TAny
                    case Some(t: TArray) =>
                        val et = if (t.entries.size > 0) {
                            t.entries.values.reduceLeft(_ union _)
                        } else {
                            TBottom
                        }

                         removeUninit(false)(et union t.global)
                    case _ =>
                        TAny
                }
            case ArrayCurElement(ar) => TAny
            case ArrayCurKey(id: SimpleVariable) =>
                env.lookup(id) match {
                    case Some(TAnyArray) =>
                        TString union TInt
                    case Some(t: TArray) =>
                        var hasIntKey = false
                        var hasStringKey = false
                        for (k <- t.entries.keys) k match {
                            case _: IntKey =>
                                hasIntKey = true
                            case _: StringKey =>
                                hasStringKey = true
                        }

                        val et = (if (hasIntKey) TInt else TBottom) union (if (hasStringKey) TString else TBottom)
                        removeUninit(false)(et union t.global)
                    case _ =>
                        TString union TInt
                }

            case ArrayCurKey(ar) => TString union TInt

            case ArrayCurIsValid(ar) =>
                TBoolean
            case New(cr, params) => cr match {
                case AST.StaticClassRef(_, _, id) =>
                    GlobalSymbols.lookupClass(id.value) match {
                        case a @ Some(cs) =>
                            allocObject(node, a)
                        case _ =>
                            error("Undefined class '"+id.value+"'", id)
                            allocObject(node, None)
                    }
                case _ =>
                    allocObject(node, None)
            }
            case cl @ Clone(obj) =>
                typeFromSV(obj) match {
                    case ref: TObjectRef =>
                        val ro = env.store.lookup(ref)
                        env = env.setStore(env.store.set(ObjectId(cl.uniqueID, ObjectIdUse), ro))
                        new TObjectRef(ObjectId(cl.uniqueID, ObjectIdUse))
                    case _ =>
                        TAnyObject
                }
            case FunctionCall(AST.Identifier("phantm_dumpanddie"), args) =>
                for (unser <- ctx.dumpedData) {
                    env = unser.heap.importToEnv(env)
                }
                TBottom

            case fcall @ FunctionCall(id, args) =>
                GlobalSymbols.lookupFunction(id.value) match {
                    case Some(fs) =>
                            if (collectAnnotations) {
                                val ft = new TFunction(args.map(a => (typeFromSV(a), false, false)), TBottom)
                                AnnotationsStore.collectFunction(fs, ft);
                            }
                            checkFCalls(fcall.params, fs, fcall)
                    case None =>
                        // handle special functions
                        id.value.toLowerCase match {
                            case "eval" =>
                                notice("eval() statements are ignored.", id)
                                TAny
                            case "isset" | "empty" =>
                                TBoolean // no need to check the args, this is a no-error function
                            case _ =>
                                notice("Function "+id.value+" appears to be undefined!", id)
                                TBottom
                        }
                }
            case mcall @ MethodCall(r, mid, args) =>
                typeFromSV(r) match {
                    case or: TObjectRef =>
                        val ro = env.store.lookup(or);
                        ro.lookupMethod(mid.value, env.scope) match {
                            case Some(ms) =>
                                if (collectAnnotations) {
                                    // TODO: Create a FunctionType and add it to the list of potential prototypes
                                }
                                checkFCalls(args, ms, mcall)
                            case None =>
                                // Check for magic __call ?
                                val cms = ro.lookupMethod("__call", env.scope)
                                if (cms == None) {
                                    notice("Undefined method '" + mid.value + "' in object "+ro, mid)
                                    TBottom
                                } else {
                                    val ms = cms.get
                                    if (ms.ftyps.size > 0) {
                                        ms.ftyps.head.ret
                                    } else {
                                        TBottom
                                    }
                                }
                        }
                    case _ =>
                        TTop
                }

            case Constant(cs) =>
                cs.typ

            case const @ ClassConstant(cs) =>
                cs.typ

            case mcall @ StaticMethodCall(cl, id, args) =>
                TAny // TODO

            case tern @ Ternary(iff, then, elze) =>
                typeFromSV(then) union typeFromSV(elze)

            case Cast(typ, v) =>
                typ match {
                    case AST.CastUnset => TNull
                    case AST.CastInt => TInt
                    case AST.CastString => TString
                    case AST.CastDouble => TFloat
                    case AST.CastArray => TAnyArray
                    case AST.CastBool => TBoolean
                    case AST.CastObject => TAnyObject
                }

            case id: SimpleVariable =>
              env.lookup(id).getOrElse(TTop)

            case ae @ ArrayEntry(ar, ind) =>
                val indtyp = typeFromSV(ind)

                typeFromSV(ar) match {
                    case TString | _: TStringLit  if leq(indtyp, TNumeric) =>
                        TString
                    case t: TArray =>
                        t.lookupByType(indtyp)
                    case u: TUnion =>
                        u.types.map { _ match {
                            case TString | _: TStringLit if leq(indtyp, TNumeric) =>
                                TString
                            case ta: TArray =>
                                ta.lookupByType(indtyp)
                            case _ =>
                                TBottom
                        }}.reduceLeft(_ union _)
                    case TAny | TTop =>
                        TTop
                    case _ =>
                        TBottom
                }
            case ae @ NextArrayEntry(arr) =>
                typeFromSV(arr) match {
                    case ta: TArray =>
                        ta.entries.collect{ case t : IntKey => t }.foldLeft(TBottom: Type)((t, e)=> t union e._2) union ta.globalInt
                    case u: TUnion =>
                        u.types.map { _ match {
                            case ta: TArray =>
                                ta.entries.collect{ case t : IntKey => t }.foldLeft(TBottom: Type)((t, e)=> t union e._2) union ta.globalInt
                            case _ =>
                                TBottom
                        }}.reduceLeft(_ union _)
                    case TAny | TTop =>
                        TTop
                    case _ =>
                        TBottom
               }

            case op @ ObjectProperty(obj, p) =>
                typeFromSV(obj) match {
                    case TAnyObject | TAny | TTop =>
                        TTop
                    case or: TPreciseObject =>
                        or.realObject(env).lookupField(p)
                    case u: TUnion =>
                        u.types.map { _ match {
                            case TAnyObject | TAny | TTop =>
                                TTop
                            case or: TPreciseObject =>
                                or.realObject(env).lookupField(p)
                            case _ =>
                                TBottom
                        }}.reduceLeft(_ union _)
                    case _ =>
                        TBottom
                }

            case vv @ VariableClassConstant(cr, id) =>
                notice("Dynamically referenced class constants ignored", vv)
                TBottom

            case vv @ VariableClassProperty(cr, prop) =>
                notice("Dynamically referenced class properties ignored", vv)
                TBottom

            case vv @ VariableVar(v) =>
                notice("Dynamic variable ignored", vv)
                TBottom

            case u =>
              println("Unknown simple value: "+u)
              TBottom
        }

        def allocObject(node: Statement, ocs: Option[ClassSymbol]): ObjectType = {
            val id = ObjectId(node.uniqueID, ObjectIdUse);
            env = envInit.store.store.get(id) match {
                case Some(o) if o.singleton =>
                    // Object becomes multiton, we merge it with a newly allocated object
                    val (nenv, obj) = TypeLattice.joinObjects(env, env.store.lookup(id), env.store.newObject(id, ocs));
                    nenv.setStore(nenv.store.set(id, obj.setMultiton))
                case _ =>
                    env.setStore(env.store.initIfNotExist(id, ocs))
            }
            new TObjectRef(id)
        }

        def typeError(pos: Positional, etyp: Type, vtyp: Type): Unit =
            typeErrorF("Type mismatch: expected: %s, found: %s", pos, etyp, vtyp)

        def typeErrorF(format: String, pos: Positional, etyp: Type, vtyp: Type): Unit = {
            if (!silent) {
                def filterErrors(t: Type): Boolean = {
                    if (Settings.get.verbosity <= 0 && possiblyUninit(t)) {
                        true
                    } else if (Settings.get.verbosity < 0 && t == TAny) {
                        true
                    } else {
                        false
                    }
                }
                def simpleText(t: Type): String = t match {
                    case ta: TArray =>
                        "Array[...]"
                    case to: TObjectRef =>
                        "Object[...]"
                    case tu: TUnion => 
                        tu.types.map(simpleText).mkString(" or ")
                    case t =>
                        t.toText(env)
                }

                def typesDiff(et: Type, vt: Type): ((String, String), Boolean) = (et,vt) match {
                    case (eta: TArray, vta: TArray) =>
                        var relevantKeys = Set[ArrayKey]();
                        var cancel = false

                        // Emphasis on the differences
                        for (k <- eta.entries.keySet ++ vta.entries.keySet) {
                            if (!leq(vta.lookup(k), eta.lookup(k))) {
                                relevantKeys = relevantKeys + k
                            }
                        }

                        var rhs, lhs = List[String]()

                        for (k <- relevantKeys) {
                            val diff = typesDiff(eta.lookup(k), vta.lookup(k))
                            lhs = k+" => "+diff._1._1 :: lhs
                            rhs = k+" => "+diff._1._2 :: rhs
                            cancel = cancel || diff._2
                        }

                        if (!leq(vta.globalString, eta.globalString)) {
                            val diff = typesDiff(vta.globalString, eta.globalString)
                            lhs = "?s => "+diff._1._1 :: lhs
                            rhs = "?s => "+diff._1._2 :: rhs
                            cancel = cancel || diff._2
                        }

                        if (!leq(vta.globalInt, eta.globalInt)) {
                            val diff = typesDiff(vta.globalInt, eta.globalInt)
                            lhs = "?i => "+diff._1._1 :: lhs
                            rhs = "?i => "+diff._1._2 :: rhs
                            cancel = cancel || diff._2
                        }

                        if (lhs.size < eta.entries.size+1) {
                            lhs = "..." :: lhs;
                        }

                        if (rhs.size < vta.entries.size+1) {
                            rhs = "..." :: rhs;
                        }

                        if (relevantKeys.forall(k => filterErrors(vta.lookup(k)))) {
                            cancel = true
                        }

                        ((lhs.reverse.mkString("Array[", ", ", "]"), rhs.reverse.mkString("Array[", ", ", "]")), cancel)
                    case (et, vt: TArray) =>
                        ((et.toText(env), simpleText(vt)), false)

                    case (eto: TPreciseObject, vto: TPreciseObject) =>
                        val ero = eto.realObject(env)
                        val vro = vto.realObject(env)

                        var relevantFields = Set[String]();
                        var cancel = false

                        // Emphasis on the differences
                        for (f <- ero.fields.keySet ++ vro.fields.keySet) {
                            if (!leq(vro.lookupField(f), ero.lookupField(f))) {
                                relevantFields += f
                            }
                        }

                        var rhs, lhs = List[String]()

                        for (f <- relevantFields) {
                            val diff = typesDiff(ero.lookupField(f), vro.lookupField(f))
                            lhs = f+" => "+diff._1._1 :: lhs
                            rhs = f+" => "+diff._1._2 :: rhs
                            cancel = cancel || diff._2
                        }

                        if (!leq(vro.globalType, ero.globalType)) {
                            val diff = typesDiff(vro.globalType, ero.globalType)
                            lhs = "? => "+diff._1._1 :: lhs
                            rhs = "? => "+diff._1._2 :: rhs
                            cancel = cancel || diff._2
                        }

                        if (lhs.size < ero.fields.size+1) {
                            lhs = "..." :: lhs;
                        }

                        if (rhs.size < vro.fields.size+1) {
                            rhs = "..." :: rhs;
                        }

                        if (relevantFields.forall(f => filterErrors(vro.lookupField(f)))) {
                            cancel = true
                        }

                        ((lhs.reverse.mkString("Object[", ", ", "]"), rhs.reverse.mkString("Object[", ", ", "]")), cancel)
                    case (et, vto: TObjectRef) =>
                        ((et.toText(env), simpleText(vto)), false)

                    case (eta: TArray, vt) =>
                        ((simpleText(eta), simpleText(vt)), filterErrors(vt))
                    case (eto: TObjectRef, vt) =>
                        ((simpleText(eto), simpleText(vt)), filterErrors(vt))
                    case (etu: TUnion, vtu: TUnion) =>
                        var relevantTypes = List[String]();

                        for (t <- vtu.types) {
                            if (!leq(t, etu)) {
                                relevantTypes = simpleText(t) :: relevantTypes;
                            }
                        }

                        if (relevantTypes.size < vtu.types.size) {
                            relevantTypes = "..." :: relevantTypes
                        }

                        ((simpleText(etu), relevantTypes.reverse.mkString(" or ")), false)
                    case _ =>
                        ((et.toText(env), vt.toText(env)), filterErrors(vt))
                }

                (etyp, vtyp) match {
                    case (et, vt) if filterErrors(vt) =>
                        if (Settings.get.verbosity > 0) {
                            pos match {
                                case sv: SimpleVariable =>
                                    notice("Potentialy uninitialized variable", pos)
                                case _ =>
                                    notice("Potentialy uninitialized value", pos)
                            }
                        }
                    case (et, TUninitialized) =>
                            pos match {
                                case sv: SimpleVariable =>
                                    notice("Uninitialized variable", pos)
                                case _ =>
                                    notice("Uninitialized value", pos)
                            }
                    case (et, vt) =>
                        val (diff, cancelError) = typesDiff(et, vt)

                        if (!cancelError) {
                            notice(String.format(format, diff._1, diff._2), pos)
                        }
                }
            }
        }

        def fixTmpObjects(t: Type, sv: SimpleValue): Type = {
            // We need to turn All TObjectTmp into TObjectRef
            var lookedAt: Set[ObjectId] = Set()
            var offset = 1
            var store  = env.store

            def fixType(t: Type) : Type = t match {
                case ta: TArray =>
                    new TArray(ta.entries.map(kt => (kt._1, fixType(kt._2))), fixType(ta.globalInt), fixType(ta.globalString))

                case tor: TObjectRef =>
                    if (lookedAt contains tor.id) {
                        tor
                    } else {
                        // check object fields
                        lookedAt += tor.id
                        var ro = store.lookup(tor)

                        ro = ro.copy(fields = ro.fields.map(kt => (kt._1, fixType(kt._2))), globalType =  fixType(ro.globalType))
                        store = store.set(tor.id, ro)

                        tor
                    }

                case tot: TObjectTmp =>

                    var ro = tot.obj
                    ro = ro.copy(fields = ro.fields.map(kt => (kt._1, fixType(kt._2))), globalType =  fixType(ro.globalType))

                    var id = new ObjectId(sv.uniqueID, ObjectIdTmp(offset))
                    offset   += 1
                    lookedAt += id

                    store = store.set(id, ro)
                    new TObjectRef(id)

                case tu: TUnion =>
                    TUnion(tu.types.map(t => fixType(t)))

                case t => t
            }

            val r = fixType(t)

            env = env.setStore(store)

            r
        }

        def expOrRef(v1: SimpleValue, typs: Type*): Type = {
            val etyp = typs reduceLeft (_ union _)
            val vtyp = typeFromSV(v1)

            def checkVariable(v: Variable): Type = {
                val (osv, svetyp, hasTmp) = getCheckType(v, etyp)

                if (!osv.isEmpty) {
                    val sv = osv.get
                    val svvtyp = typeFromSV(sv)

                    var svtypCheck  = svvtyp

                    if (leq(svvtyp, svetyp)) {
                        vtyp
                    } else {
                        typeError(sv, svetyp, svvtyp)

                        var t = meet(svetyp, svvtyp)

                        if (hasTmp) {
                            t = fixTmpObjects(t, sv)
                        }

                        env = env.inject(sv, t)


                        // we then return the type
                        // Note: Since etyp will never be TmpObject, the result cannot be TMP Obj
                        meet(etyp, vtyp)
                    }
                } else {
                    TTop
                }
            }

            v1 match {
                case sv: SimpleVariable =>
                    checkVariable(sv)
                case v: NextArrayEntry =>
                    checkVariable(v)
                case v: ArrayEntry =>
                    checkVariable(v)
                case v: ObjectProperty =>
                    checkVariable(v)
                case v =>
                    if (leq(vtyp, etyp)) {
                        vtyp
                    } else {
                        typeError(v, etyp, vtyp)
                    }
                    meet(etyp, vtyp)

            }
        }

        def typeFromUnOP(op: UnaryOperator, v1: SimpleValue): Type = op match {
            case BOOLEANNOT =>
                expOrRef(v1, TAny)
            case BITSIWENOT =>
                expOrRef(v1, TInt)
            case PREINC =>
                expOrRef(v1, TInt)
            case POSTINC =>
                expOrRef(v1, TInt)
            case PREDEC =>
                expOrRef(v1, TInt)
            case POSTDEC =>
                expOrRef(v1, TInt)
            case SILENCE =>
                expOrRef(v1, TAny)
        }

        def typeFromBinOP(v1: SimpleValue, op: BinaryOperator, v2: SimpleValue): Type = op match {
            case PLUS =>
                val t1 = typeFromSV(v1)

                if (leq(t1, TAnyArray)) {
                    expOrRef(v2, TAnyArray)
                } else {
                    expOrRef(v1, TNumeric) union expOrRef(v2, TNumeric)
                }
            case MINUS | MULT =>
                expOrRef(v2, TNumeric) union expOrRef(v1, TNumeric)

            case MOD =>
                expOrRef(v2, TNumeric)
                expOrRef(v1, TNumeric)
                TInt

            case DIV =>
                expOrRef(v2, TNumeric)
                expOrRef(v1, TNumeric)
                TNumeric
            case CONCAT =>
                expOrRef(v1, TAny)
                expOrRef(v2, TAny)
                TString
            case INSTANCEOF =>
                expOrRef(v1, TAnyObject)
                expOrRef(v2, TString)
                TBoolean
            case BITWISEAND | BITWISEOR | BITWISEXOR =>
                expOrRef(v1, TInt)
                expOrRef(v2, TInt)
            case SHIFTLEFT | SHIFTRIGHT =>
                expOrRef(v1, TInt)
                expOrRef(v2, TInt)
            case BOOLEANAND | BOOLEANOR | BOOLEANXOR | LT | LEQ | GEQ | GT |
                 EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL=>
                expOrRef(v1, TAny)
                expOrRef(v2, TAny)
                TBoolean
        }

        def getCheckType(sv: SimpleValue, ct: Type, hasTmp: Boolean = false): (Option[SimpleVariable], Type, Boolean) = sv match {
            case VariableVar(v) =>
                getCheckType(v, TString, hasTmp)
            case ArrayEntry(arr, index) =>
                typeFromSV(arr) match {
                    case TString | _ :TStringLit =>
                        // If arr is known to be a string, index must be Int
                        expOrRef(index, TNumeric)
                        getCheckType(arr, TString, hasTmp)
                    case to: ObjectType =>
                        getCheckType(arr, TAny, hasTmp)
                    case t =>
                        expOrRef(index, TString, TNumeric)
                        val newct = if (ct == TTop) {
                            TAnyArray
                        } else {
                            typeFromSV(index) match {
                                case TStringLit(v) =>
                                    new TArray().setAny(TTop).inject(ArrayKey.fromString(v), ct)
                                case TIntLit(v) =>
                                    new TArray().setAny(TTop).inject(IntKey(v), ct)
                                case TFloatLit(v) =>
                                    new TArray().setAny(TTop).inject(IntKey(v.toLong), ct)
                                case _ =>
                                    new TArray().setAny(ct)
                            }
                        }
                        getCheckType(arr, newct, hasTmp)
                }
            case NextArrayEntry(arr) =>
                getCheckType(arr, TAnyArray, hasTmp)
            case ObjectProperty(obj, prop) =>
                var hasTmpNew = false
                val newct = if (ct == TTop) {
                    TAnyObject
                } else {
                    val ro = typeFromSV(prop) match {
                        case TStringLit(v) =>
                            new TRealObject(Map[String, Type]() + (v -> ct), TTop, true, TAnyClass)
                        case _ =>
                            new TRealObject(Map(), ct, true, TAnyClass)
                    }
                    hasTmpNew = true
                    new TObjectTmp(ro)
                }

                getCheckType(obj, newct, hasTmpNew)
            case svar: SimpleVariable =>
                (Some(svar), ct, hasTmp)
            case VariableClassConstant(cr, id) =>
                (None, ct, hasTmp)
            case VariableClassProperty(cr, id) =>
                (None, ct, hasTmp)
            case NoVar() =>
                (None, ct, hasTmp)
            case v =>
                Predef.error("Woops, unexpected Variable("+v+") inside checktype of!")
        }

        def assign(v: Variable, ext: Type): Type = {
            val (osvar, ct, hasTmp) = getCheckType(v, TTop)
            if (!osvar.isEmpty) {
                val svar = osvar.get
                //println("Assigning "+v+" to "+ext)
                //println("Checking "+svar+"("+typeFromSV(svar)+") against "+ct)
                var reft = expOrRef(svar, ct)
                //println("After refinement: "+reft)

                // Now, we need to get down in that variable and affect the type as the assign require
                def backPatchType(sv: SimpleValue, typ: Type): Type = sv match {
                    case VariableVar(v) =>
                        backPatchType(v, TString)
                    case ArrayEntry(arr, index) =>
                        val indtyp = typeFromSV(index)

                        val t = typeFromSV(arr) match {
                            case ta: TArray =>
                                ta.injectByType(indtyp, typ)
                            case tu: TUnion =>
                                val typs = for (f <- tu.types) yield f match {
                                    case ta: TArray =>
                                        ta.injectByType(indtyp, typ)
                                    case t =>
                                        t
                                }

                                typs.foldLeft(TBottom: Type)(_ union _)
                            case TAny =>
                                TAny
                            case TTop =>
                                TTop
                            case _ =>
                                TBottom
                        }
                        backPatchType(arr, t)
                    case NextArrayEntry(arr) =>
                        val t = typeFromSV(arr) match {
                            case ta: TArray =>
                                ta.setAnyInt(typ union ta.globalInt)
                            case tu: TUnion =>
                                val typs = for (f <- tu.types) yield f match {
                                    case ta: TArray =>
                                        ta.setAnyInt(typ union ta.globalInt)
                                    case t =>
                                        t
                                }

                                typs.foldLeft(TBottom: Type)(_ union _)
                            case TAny =>
                                TAny
                            case TTop =>
                                TTop
                            case _ =>
                                TBottom
                        }
                        backPatchType(arr, t)
                    case ObjectProperty(obj, prop) =>
                        def updateObject(obj: TObjectRef) {
                            val ro = obj.realObject(env).injectField(prop, typ)
                            env = env.setObject(obj.id, ro)
                        }
                        val t = typeFromSV(obj) match {
                            case to: TObjectRef =>
                                updateObject(to)
                                to
                            case to: TObjectTmp =>
                                val ro = to.obj.injectField(prop, typ)
                                new TObjectTmp(ro)
                            case tu: TUnion =>
                                for (f <- tu.types) f match {
                                    case to: TObjectRef =>
                                        updateObject(to)
                                    case TAnyObject =>
                                        // TODO
                                    case _ =>
                                }

                                tu
                            case TAnyObject =>
                                // We need to create one object here
                                var id = new ObjectId(obj.uniqueID, ObjectIdUse)
                                var ro = new TRealObject(Map(), TTop, false, TAnyClass).injectField(prop, typ, false)
                                env = env.setObject(id, ro)
                                new TObjectRef(id)

                            case TAny =>
                                TAny
                            case TTop =>
                                TTop
                            case _ =>
                                TBottom
                        }
                        backPatchType(obj, t)
                    case svar: SimpleVariable =>
                        typ

                    case _ =>
                        Predef.error("Woops, unexpected Variable inside checktype of!")
                }

                def limitType(typ: Type, l: Int): Type = typ match {
                    case ta: TArray =>
                        if (l == 0) {
                            TAnyArray
                        } else {
                            new TArray(Map[ArrayKey, Type]() ++ ta.entries.map(e => (e._1, limitType(e._2, l-1))), 
                                       limitType(ta.globalInt, l-1),
                                       limitType(ta.globalString, l-1)
                                       )
                        }
                    case to: TObjectRef =>
                        if (l == 0) {
                            TAnyObject
                        } else {
                            to
                        }
                    case tu: TUnion =>
                        if (l == 0) {
                            TTop
                        } else {
                            tu.types.map(limitType(_, l-1)).reduceLeft(_ union _)
                        }
                    case t =>
                        if (l == 0) {
                            TTop
                        } else {
                            t
                        }
                }

                if (hasTmp) {
                    reft = fixTmpObjects(reft, svar)
                }
                //println("Refined type: "+reft)
                env = env.inject(svar, reft)

                var rest = backPatchType(v, ext)

                if (rest.depth(env) >= 5) {
                    rest = limitType(rest, 5)
                }
                if (hasTmp) {
                    rest = fixTmpObjects(rest, svar)
                }
                //println("Backpatched type: "+rest)
                env = env.inject(svar, rest)

                rest
            } else {
                ext
            }
        }

        def checkFCalls(fcall_params: List[SimpleValue], sym: FunctionSymbol, pos: Positional) : Type =  {
            def protoErrors(ftyp: FunctionType): Int = {
                ftyp match {
                    case tf: TFunction =>
                        var ret = 0;
                        for (i <- fcall_params.indices) {
                            if (i >= tf.args.length) {
                                ret += 1
                            } else {
                                if (!leq(typeFromSV(fcall_params(i)), tf.args(i)._1)) {
                                    //notice("Prototype mismatch because "+fcall.params(i)+"("+typeFromSV(fcall.params(i))+") </: "+args(i)._1) 

                                    ret += 1;
                                }
                            }
                        }
                        ret
                    case TFunctionAny =>
                        0
                }
            }

            def getInlinedRetType(params: List[Type]): Type = ctx.cfgs.get(Some(sym)) match {
                case Some(cfg) =>
                    val gr = ctx.results

                    if (gr.inlineCache(sym) contains params) {
                        // cache hit
                        val (t, store) = gr.inlineCache(sym)(params)
                        env = env unionStore store
                        t
                    } else {
                        if (Settings.get.displayProgress && Settings.get.verbosity <= 2) {
                            Reporter.get.tick
                        }

                        // clear & save previous annotations
                        val annots = AnnotationsStore.clearFunctionAnnotations(sym)

                        // save general argument types and inject call types
                        val oldTypes = sym.argList.map(a => a._2.typ)

                        for ((t, i) <- params.zipWithIndex) {
                            if (i < sym.argList.size) {
                                sym.argList(i)._2.typ = t
                            }
                        }

                        // analyze like usual
                        val tfa = new TypeFlowAnalyzer(cfg, sym, ctx, true, false, new TypeEnvironment unionStore env.store)
                        val res = tfa.analyze

                        val rStore = res(cfg.exit).store

                        // retreive return type
                        val rtyp = AnnotationsStore.getReturnType(sym)

                        env = env unionStore rStore

                        // restore annotations
                        AnnotationsStore.restoreFunctionAnnotations(sym, annots)

                        // restore argument types
                        for ((t, i) <- oldTypes.zipWithIndex) {
                            sym.argList(i)._2.typ = t
                        }

                        gr.inlineCache = gr.inlineCache + (sym -> (gr.inlineCache(sym) + (params -> (rtyp, rStore))))

                        rtyp
                    }
                case None =>
                    error("Unable to retrieve target CFG to inline", pos)
                    TBottom
            }

            def checkAgainstFType(ftyp: FunctionType): Type = ftyp match {
                case tf: TFunction =>
                    for (i <- fcall_params.indices) {
                        if (i >= tf.args.length) {
                            error("Prototype error!", pos)
                        } else {
                            (fcall_params(i), tf.args(i)._1, tf.args(i)._2) match {
                                case (v: Variable, etyp, true) =>
                                    // If by ref and variable, we assign it directly
                                    assign(v, etyp)
                                case (sv, etyp, byref) =>
                                    expOrRef(sv, etyp)
                            }
                        }

                    }

                    if (sym.shouldInline) {
                        getInlinedRetType(fcall_params.map(typeFromSV(_)))
                    } else {
                        tf.ret
                    }
                case _ =>
                    TAny
            }

            // If necessary, we record global types into the CTX
            if (collectGlobals && sym.userland) {
                val gr = ctx.results
                gr.globalCalls += (sym -> (gr.globalCalls(sym) + (pos.getPos -> env)))
            }

            val selectedFTyp = sym.ftyps.map(f => (f, protoErrors(f))).toSeq.sortWith((a,b) => (a._2 < b._2)).head._1

            checkAgainstFType(selectedFTyp)
        }

        def filterType(v: Variable, reft: Type) = {
            val t = typeFromSV(v);

            if (t != TBottom) {
                // We don't want to generate "unreachable code"
                // if the type is already bottom

                val rest = meet(t, reft)

                if (rest == TBottom) {
                    // unreachable code
                    env = BaseTypeEnvironment
                } else {
                    val (osv, ct, hasTmp) = getCheckType(v, rest)
                    if (!osv.isEmpty) {
                        var t = meet(typeFromSV(osv.get), ct)
                        if (hasTmp) {
                            t = fixTmpObjects(t, v)
                        }
                        env = env.inject(osv.get, t)
                    }
                }
            }
        }

        node match {
            case Assign(vr: Variable, v1) =>
                //println("Assign..")
                val t = expOrRef(v1, TAny)
                assign(vr, uninitToNull(t))

            case AssignUnary(vr: Variable, op, v1) =>
                // We want to typecheck v1 according to OP
                val t = typeFromUnOP(op, v1);
                assign(vr, uninitToNull(t))

            case AssignBinary(vr: Variable, v1, op, v2) =>
                // We want to typecheck v1/v2 according to OP
                val t = typeFromBinOP(v1, op, v2)
                assign(vr, uninitToNull(t))

            case AssumeProperty(p, vs) =>
                p match {
                    case Isset =>
                        vs.foreach(v => filterType(v, TAny))
                    case Empty =>
                        vs.foreach(v => filterType(v, falseTypes))
                    case IsInt =>
                        filterType(vs.head, TInt)
                    case IsFloat =>
                        filterType(vs.head, TFloat)
                    case IsArray =>
                        filterType(vs.head, TAnyArray)
                    case IsResource =>
                        filterType(vs.head, TResource)
                    case IsString =>
                        filterType(vs.head, TString)
                    case IsObject =>
                        filterType(vs.head, TAnyObject)
                    case IsBool =>
                        filterType(vs.head, TBoolean)
                    case IsNull =>
                        filterType(vs.head, TNull)
                    case IsScalar =>
                        filterType(vs.head, TNumeric union TBoolean union TString)
                }

            case AssumeNotProperty(p, vs) =>
                p match {
                    case Isset =>
                        if  (vs.size == 1) {
                            filterType(vs.head, TUninitialized union TNull)
                        } else {
                            // can't do anything
                        }
                    case Empty =>
                        filterType(vs.head, trueTypes)
                    case IsInt =>
                        filterType(vs.head, allTypesBut(TInt))
                    case IsFloat =>
                        filterType(vs.head, allTypesBut(TFloat))
                    case IsArray =>
                        filterType(vs.head, allTypesBut(TAnyArray))
                    case IsResource =>
                        filterType(vs.head, allTypesBut(TResource))
                    case IsString =>
                        filterType(vs.head, allTypesBut(TString))
                    case IsObject =>
                        filterType(vs.head, allTypesBut(TAnyObject))
                    case IsBool =>
                        filterType(vs.head, allTypesBut(TFalse, TTrue))
                    case IsNull =>
                        filterType(vs.head, allTypesBut(TNull))
                    case IsScalar =>
                        filterType(vs.head, TAnyObject union TAnyArray union TResource)
                }

            case Assume(v1, op, v2) => op match {
                case LT | LEQ | GEQ | GT =>
                    expOrRef(v1, TNumeric)
                    expOrRef(v2, TNumeric)
                case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                    expOrRef(v1, TAny)
                    expOrRef(v2, TAny)

                    def filterOType(v: Variable, reft: Option[Type]) =
                        if (!reft.isEmpty) filterType(v, reft.get);


                    def getReprTypes(sv: StaticValue, bval: Boolean, strict: Boolean): Option[Type] = (sv, bval, strict) match {
                        case (sv,          true,    true)  => Some(typeFromSV(sv))
                        case (_:PHPTrue,   true,    _)     => Some(trueTypes)
                        case (_:PHPTrue,   false,   false) => Some(falseTypes)
                        case (_:PHPFalse,  true,    _)     => Some(falseTypes)
                        case (_:PHPFalse,  false,   false) => Some(trueTypes)
                        case (_:PHPNull,   true,    false) => Some(falseTypes)
                        case (_:PHPNull,   false,   false) => Some(trueTypes)
                        // Exclude singleton types from var in var !== <val>§
                        case (_:PHPNull,   false,   true)  => Some(allTypesBut(TNull))
                        case (_:PHPTrue,   false,   true)  => Some(allTypesBut(TTrue))
                        case (_:PHPFalse,  false,   true)  => Some(allTypesBut(TFalse))
                        case _ => None
                    }

                    (v1, op, v2) match {
                        case (v: Variable, EQUALS, sv: StaticValue) =>
                            filterOType(v, getReprTypes(sv, true, false))
                        case (v: Variable, IDENTICAL, sv: StaticValue) =>
                            filterOType(v, getReprTypes(sv, true, true))
                        case (v: Variable, NOTEQUALS, sv: StaticValue) =>
                            filterOType(v, getReprTypes(sv, false, false))
                        case (v: Variable, NOTIDENTICAL, sv: StaticValue) =>
                            filterOType(v, getReprTypes(sv, false, true))
                        case (sv: StaticValue, EQUALS, v: Variable) =>
                            filterOType(v, getReprTypes(sv, true, false))
                        case (sv: StaticValue, IDENTICAL, v: Variable) =>
                            filterOType(v, getReprTypes(sv, true, true))
                        case (sv: StaticValue, NOTEQUALS, v: Variable) =>
                            filterOType(v, getReprTypes(sv, false, false))
                        case (sv: StaticValue, NOTIDENTICAL, v: Variable) =>
                            filterOType(v, getReprTypes(sv, false, true))
                        case _ =>
                            // no filtering
                    }
              }

            case Print(v) =>
                expOrRef(v, TAny)

            case r @ Return(v) =>
                ctx.symbol match {
                    case Some(fs: FunctionSymbol) =>

                        val retType = if (!fs.ftyps.isEmpty) fs.ftyps.map(_.ret).reduceLeft(_ union _) else TTop
                        val vType = typeFromSV(v)

                        if (!leq(vType, retType)) {
                            typeErrorF("Return type mismatch: expected: %s, found: %s", r, retType, vType)
                        }

                        if (collectAnnotations || inlined) {
                            AnnotationsStore.collectFunctionRet(fs, vType)
                        }
                    case _ =>
                }

            case Unset(v) =>
                assign(v, TUninitialized)

            case ex: SimpleValue =>
                expOrRef(ex, TAny)

            case Skip =>

            case _ => notice(node+" not yet handled", node)
        }

        env
    }
}
