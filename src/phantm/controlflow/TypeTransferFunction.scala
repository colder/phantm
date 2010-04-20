package phantm.controlflow

import phantm.Main
import phantm.util.{Positional, Reporter}
import phantm.AST.{Trees => AST}
import phantm.CFG.ControlFlowGraph
import phantm.CFG.Trees._
import phantm.symbols._
import phantm.types._

case class TypeTransferFunction(silent: Boolean, collectAnnotations: Boolean) extends TransferFunction[TypeEnvironment, Statement] {
    def notice(msg: String, pos: Positional) = if (!silent) Reporter.notice(msg, pos)
    def error(msg: String, pos: Positional) = if (!silent) Reporter.error(msg, pos)

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
                new TArray(Map[String, Type]() ++ ta.entries.map{ e => (e._1, removeUninit(removeInArrays)(e._2)) }, removeUninit(removeInArrays)(ta.globalType))
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

        def leq(t1: Type, t2: Type) = TypeLattice.leq(env, env, t1, t2)
        def meet(t1: Type, t2: Type) = {
            val (nenv, t) = TypeLattice.meet(env, env, t1, t2)
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
            case PHPThis()           => getObject(node, env.scope)
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

                         removeUninit(false)(et union t.globalType)
                    case _ =>
                        TAny
                }
            case ArrayCurElement(ar) => TAny
            case ArrayCurKey(ar)     => TString union TInt
            case ArrayCurIsValid(ar) =>
                TBoolean
            case New(cr, params) => cr match {
                case AST.StaticClassRef(_, _, id) =>
                    GlobalSymbols.lookupClass(id.value) match {
                        case a @ Some(cs) =>
                            getObject(node, a)
                        case _ =>
                            error("Undefined class '"+id.value+"'", id)
                            getObject(node, None)
                    }
                case _ =>
                    getObject(node, None)
            }
            case cl @ Clone(obj) =>
                typeFromSV(obj) match {
                    case ref: TObjectRef =>
                        val ro = env.store.lookup(ref)
                        env = env.setStore(env.store.set(ObjectId(cl.uniqueID, 0), ro))
                        new TObjectRef(ObjectId(cl.uniqueID, 0))
                    case _ =>
                        TAnyObject
                }
            case FunctionCall(AST.Identifier("phantm_dumpanddie"), args) =>
                if (Main.dumpedData != Nil) {
                    for (unser <- Main.dumpedData) {
                        env = unser.importToEnv(env)
                    }
                }
                TBottom

            case fcall @ FunctionCall(id, args) =>
                GlobalSymbols.lookupFunction(id.value) match {
                    case Some(fs) =>
                            if (collectAnnotations) {
                                val ft = new TFunction(args.map(a => (typeFromSV(a), false)), TBottom)
                                AnnotationsStore.collectFunction(fs, ft);
                            }
                            checkFCalls(fcall.params, fs.ftyps.toList, fcall)
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
                            case Some(mt) =>
                                if (collectAnnotations) {
                                    // TODO: Create a FunctionType and add it to the list of potential prototypes
                                }
                                checkFCalls(args, List(mt), mcall)
                            case None =>
                                // Check for magic __call ?
                                val cms = ro.lookupMethod("__call", env.scope)
                                if (cms == None) {
                                    notice("Undefined method '" + mid.value + "' in object "+ro, mid)
                                    TBottom
                                } else {
                                    cms.get.ret
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
                    case t: TArray =>
                        t.lookupByType(indtyp)
                    case u: TUnion =>
                        u.types.map { _ match {
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
                        ta.entries.foldLeft(TBottom: Type)((t, e)=> t union e._2) union ta.globalType
                    case u: TUnion =>
                        u.types.map { _ match {
                            case ta: TArray =>
                                ta.entries.foldLeft(TBottom: Type)((t, e)=> t union e._2) union ta.globalType
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
                    case or: TObjectRef =>
                        env.store.lookup(or).lookupField(p)
                    case u: TUnion =>
                        u.types.map { _ match {
                            case TAnyObject | TAny | TTop =>
                                TTop
                            case or: TObjectRef =>
                                env.store.lookup(or).lookupField(p)
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

        def getObject(node: Statement, ocs: Option[ClassSymbol]): ObjectType = {
            val id = ObjectId(node.uniqueID, 0);
            env = env.setStore(env.store.initIfNotExist(id, ocs))
            new TObjectRef(id)
        }

        def typeError(pos: Positional, etyp: Type, vtyp: Type): Unit = {
            if (!silent) {
                def filterErrors(t: Type): Boolean = {
                    if (Main.verbosity <= 0 && possiblyUninit(t)) {
                        true
                    } else if (Main.verbosity < 0 && t == TAny) {
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
                        var relevantKeys = Set[String]();
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

                        if (!leq(vta.globalType, eta.globalType)) {
                            val diff = typesDiff(vta.globalType, eta.globalType)
                            lhs = "? => "+diff._1._1 :: lhs
                            rhs = "? => "+diff._1._2 :: rhs
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
                        if (Main.verbosity > 0) {
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
                            notice("Potential type mismatch: expected: "+diff._1+", found: "+diff._2, pos)
                        }
                }
            }
        }

        def expOrRef(v1: SimpleValue, typs: Type*): Type = {
            val etyp = typs reduceLeft (_ union _)
            val vtyp = typeFromSV(v1)

            def checkVariable(v: Variable, kind: String): Type = {
                val (osv, svetyp) = getCheckType(v, etyp)

                if (!osv.isEmpty) {
                    val sv = osv.get
                    val svvtyp = typeFromSV(sv)

                    var svtypCheck  = svvtyp

                    if (leq(svvtyp, svetyp)) {
                        vtyp
                    } else {
                        typeError(sv, svetyp, svvtyp)

                        val t = meet(svetyp, svvtyp)
                        //println("== Refining "+svvtyp+" to "+t+" ("+svetyp+")")
                        env = env.inject(sv, t)
                        // we then return the type
                        meet(etyp, vtyp)
                    }
                } else {
                    TTop
                }
            }

            v1 match {
                case sv: SimpleVariable =>
                    checkVariable(sv, "variable")
                case v: NextArrayEntry =>
                    checkVariable(v, "array entry")
                case v: ArrayEntry =>
                    checkVariable(v,"array entry")
                case v: ObjectProperty =>
                    checkVariable(v, "object property")
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
                    expOrRef(v1, TNumeric)
                    expOrRef(v2, TNumeric)
                }
            case MINUS | MULT | DIV | MOD =>
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

        def getCheckType(sv: SimpleValue, ct: Type): (Option[SimpleVariable], Type) = sv match {
            case VariableVar(v) =>
                getCheckType(v, TString)
            case ArrayEntry(arr, index) =>
                typeFromSV(arr) match {
                    case TString =>
                        // If arr is known to be a string, index must be Int
                        expOrRef(index, TInt)
                        getCheckType(arr, TString)
                    case to: ObjectType =>
                        getCheckType(arr, TAny)
                    case t =>
                        expOrRef(index, TString, TInt)
                        val newct = if (ct == TTop) {
                            TAnyArray
                        } else {
                            typeFromSV(index) match {
                                case TStringLit(v) =>
                                    new TArray().setAny(TTop).inject(v, ct)
                                case TIntLit(v) =>
                                    new TArray().setAny(TTop).inject(v+"", ct)
                                case TFloatLit(v) =>
                                    new TArray().setAny(TTop).inject(v.toInt+"", ct)
                                case _ =>
                                    new TArray().setAny(ct)
                            }
                        }
                        getCheckType(arr, newct)
                }
            case NextArrayEntry(arr) =>
                getCheckType(arr, TAnyArray)
            case ObjectProperty(obj, prop) =>
                // IGNORE for now, focus on arrays
                getCheckType(obj, TAnyObject)
            case svar: SimpleVariable =>
                (Some(svar), ct)
            case VariableClassConstant(cr, id) =>
                (None, ct)
            case VariableClassProperty(cr, id) =>
                (None, ct)
            case NoVar() =>
                (None, ct)
            case v =>
                Predef.error("Woops, unexpected Variable("+v+") inside checktype of!")

        }

        def assign(v: Variable, ext: Type): Type = {
            val (osvar, ct) = getCheckType(v, TTop)
            if (!osvar.isEmpty) {
                val svar = osvar.get
                //println("Assigning "+v+" to "+ext)
                //println("Checking "+svar+"("+typeFromSV(svar)+") against "+ct)
                val reft = expOrRef(svar, ct)
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
                                ta.setAny(typ union ta.globalType)
                            case tu: TUnion =>
                                val typs = for (f <- tu.types) yield f match {
                                    case ta: TArray =>
                                        ta.setAny(typ union ta.globalType)
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
                            val ro =
                                if (obj.id.pos < 0) {
                                    // $this is always using strong updates
                                    obj.realObject(env).injectField(prop, typ)
                                } else {
                                    obj.realObject(env).injectField(prop, typ union obj.realObject(env).lookupField(prop))
                                }
                            env = env.setObject(obj.id, ro)
                        }
                        val t = typeFromSV(obj) match {
                            case to: TObjectRef =>
                                updateObject(to)
                                to
                            case tu: TUnion =>
                                for (f <- tu.types) f match {
                                    case to: TObjectRef =>
                                        updateObject(to)
                                    case _ =>
                                }

                                tu
                            case TAnyObject =>
                                TAnyObject
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
                            new TArray(Map[String, Type]() ++ ta.entries.map(e => (e._1, limitType(e._2, l-1))), limitType(ta.globalType, l-1))
                        }
                    case to: TObjectRef =>
                        if (l == 0) {
                            TAnyObject
                        } else {
                            // TODO
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

                env = env.inject(svar, reft)
                //println("Refined type: "+reft)
                var rest = backPatchType(v, ext)
                //println("Backpatched type: "+rest)
                //println("Depth: "+rest.depth(env))
                if (rest.depth(env) >= 5) {
                    rest = limitType(rest, 5)
                }
                //println("Limitted: "+rest)
                env = env.inject(svar, rest)
                rest
            } else {
                ext
            }
        }

        def checkFCalls(fcall_params: List[SimpleValue], syms: List[FunctionType], pos: Positional) : Type =  {
            def protoFilter(sym: FunctionType): Boolean = {
                sym match {
                    case tf: TFunction =>
                        var ret = true;
                        for (i <- fcall_params.indices) {
                            if (i >= tf.args.length) {
                                ret = false
                            } else {
                                if (!leq(typeFromSV(fcall_params(i)), tf.args(i)._1)) {
                                    //notice("Prototype mismatch because "+fcall.params(i)+"("+typeFromSV(fcall.params(i))+") </: "+args(i)._1) 

                                    ret = false;
                                }
                            }
                        }
                        ret
                    case TFunctionAny =>
                        true
                }
            }

            syms filter protoFilter match {
                case Nil =>
                    if (syms.size > 1) {
                        error("Unmatched function prototype '("+fcall_params.map(x => typeFromSV(x)).mkString(", ")+")', candidates are:\n    "+syms.mkString(",\n    "), pos)
                        TBottom
                    } else {
                        syms.head match {
                            case tf: TFunction =>
                                for (i <- fcall_params.indices) {
                                    if (i >= tf.args.length) {
                                        error("Prototype error!", pos)
                                    } else {
                                        expOrRef(fcall_params(i), tf.args(i)._1)
                                    }

                                }
                                tf.ret
                            case s =>
                                s.ret
                        }
                    }

                case f :: xs =>
                    f.ret
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

            case Assume(v1, op, v2) => op match {
                case LT | LEQ | GEQ | GT =>
                    expOrRef(v1, TNumeric)
                    expOrRef(v2, TNumeric)
                case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                    def filter(v: Variable, value: Boolean) = {
                        val t = typeFromSV(v);

                        if (t != TBottom) {
                            // We don't want to generate "unreachable code"
                            // if the type is already bottom
                            val reft = if (value == true) {
                                // possible types of $v after $v == true
                                TNumeric union TAnyArray union TString union TTrue union TResource union TAnyObject
                            } else {
                                // possible types of $v after $v == false
                                TNumeric union TAnyArray union TString union TFalse union TNull union TUninitialized
                            }

                            val rest = meet(t, reft)

                            if (rest == TBottom) {
                                // unreachable code
                                env = BaseTypeEnvironment
                            } else {
                                val (osv, ct) = getCheckType(v, rest)
                                if (!osv.isEmpty) {
                                    env = env.inject(osv.get, ct)
                                }
                            }
                        }
                    }

                    expOrRef(v1, TAny)
                    expOrRef(v2, TAny)

                    (v1, op, v2) match {
                        case (v: Variable, EQUALS | IDENTICAL, _: PHPTrue) =>
                            filter(v, true)
                        case (v: Variable, NOTEQUALS | NOTIDENTICAL, _: PHPTrue) =>
                            filter(v, false)
                        case (v: Variable, EQUALS | IDENTICAL, _: PHPFalse) =>
                            filter(v, false)
                        case (v: Variable, NOTEQUALS | NOTIDENTICAL, _: PHPFalse) =>
                            filter(v, true)
                        case (_: PHPTrue, EQUALS | IDENTICAL, v: Variable) =>
                            filter(v, true)
                        case (_: PHPTrue, NOTEQUALS | NOTIDENTICAL, v: Variable) =>
                            filter(v, false)
                        case (_: PHPFalse, EQUALS | IDENTICAL, v: Variable) =>
                            filter(v, false)
                        case (_: PHPFalse, NOTEQUALS | NOTIDENTICAL, v: Variable) =>
                            filter(v, true)
                        case _ =>
                            // no filtering
                    }
              }

            case Print(v) =>
                expOrRef(v, TAny)

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
