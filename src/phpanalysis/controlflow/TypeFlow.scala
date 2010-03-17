package phpanalysis.controlflow

import CFGTrees._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.Map
import analyzer.Symbols._
import analyzer.Types._

object TypeFlow {
    case object TypeLattice extends Lattice[TypeEnvironment] {
        type E = Type

        def leq(tex: TypeEnvironment, tey: TypeEnvironment, x : Type, y : Type): Boolean = (x,y) match {
            case (x, y) if x == y => true

            case (TBottom, _) => true
            case (_, TTop) => true
            case (_:ConcreteType, TAny) => true
            case (TTrue, TBoolean) => true
            case (TFalse, TBoolean) => true
            case (t1: TObjectRef, TAnyObject) => true
            case (t1: TObjectRef, t2: TObjectRef) =>
                val r1 = tex.store.lookup(t1)
                val r2 = tey.store.lookup(t2)

                val classesMatch = (r1, r2) match {
                    case (r1: TRealClassObject, r2: TRealClassObject) =>
                        r1.cl isSubtypeOf r2.cl
                    case (r1: TRealObject, r2: TRealClassObject) =>
                        false
                    case _ =>
                        true
                }

                classesMatch && leq(tex, tey, r1.globalType, r2.globalType) && ((r1.fields.keySet ++ r2.fields.keySet) forall (k =>
                    TypeLattice.leq(tex, tey, r1.lookupField(k), r1.lookupField(k))))

            case (t1: TArray, t2: TArray) =>
                leq(tex, tey, t1.globalType, t2.globalType) && ((t1.entries.keySet ++ t2.entries.keySet) forall (k =>
                    TypeLattice.leq(tex, tey, t1.lookup(k), t2.lookup(k))))

            case (t1: TUnion, t2: TUnion) =>
                t1.types forall { x => t2.types.exists { y => leq(tex, tey, x, y) } }
            case (t1, t2: TUnion) =>
                t2.types exists { x => leq(tex, tey, t1, x) }
            case (t1: TUnion, t2) =>
                t1.types forall { x => leq(tex, tey, x, t2) }
            case _ => false
        }

        val top = TTop
        val bottom = TBottom

        def join(x : Type, y : Type): Type = (x,y) match {
            case (TTop, _) => TTop
            case (_, TTop) => TTop

            case (TBottom, _) => y
            case (_, TBottom) => x

            case (TAny, TUninitialized) => TTop
            case (TUninitialized, TAny) => TTop

            case (TAny, _: ConcreteType) => TAny
            case (_: ConcreteType, TAny) => TAny

            case (TAny, tu: TUnion) =>
                if (!(tu.types contains TUninitialized)) {
                    TAny
                } else {
                    TTop
                }
            case (tu: TUnion, TAny) =>
                if (!(tu.types contains TUninitialized)) {
                    TAny
                } else {
                    TTop
                }

            case (TTrue, TFalse) => TBoolean
            case (TFalse, TTrue) => TBoolean
            case (TTrue, TBoolean) => TBoolean
            case (TBoolean, TTrue) => TBoolean
            case (TFalse, TBoolean) => TBoolean
            case (TBoolean, TFalse) => TBoolean

            case (t1, t2) if t1 == t2 => t1

            // Objects
            case (TAnyObject, t: TObjectRef) => TAnyObject
            case (t: TObjectRef, TAnyObject) => TAnyObject
            case (t1: TObjectRef, t2: TObjectRef) =>
                // We have a TUnion here since we are dealing
                // with objects of different refs
                TUnion(t1, t2)

            // Arrays
            case (TAnyArray, t: TArray) => TAnyArray
            case (t: TArray, TAnyArray) => TAnyArray
            case (t1: TArray, t2: TArray) =>
                var newEntries = Map[String, Type]();

                for (k <- t1.entries.keySet ++ t2.entries.keySet) {
                    newEntries = newEntries.update(k, t1.lookup(k) union t2.lookup(k))
                }

                new TArray(newEntries, t1.globalType union t2.globalType)
            // Unions
            case (t1, t2) => TUnion(t1, t2)
        }

        // unused
        def meet(x : Type, y : Type) = x
    }

    object BaseTypeEnvironment extends TypeEnvironment(HashMap[CFGSimpleVariable, Type](), None, new ObjectStore) {
        override def union(e: TypeEnvironment) = {
            e
        }

        override def equals(e: Any) = {
            if (e.isInstanceOf[AnyRef]) {
                BaseTypeEnvironment eq e.asInstanceOf[AnyRef]
            } else {
                false
            }
        }

        override def copy: TypeEnvironment =
            this

        override def toString = {
            "<base>"
        }

    }

    object AnnotationsStore {
        var functions = HashMap[String, (List[TFunction], Type)]();

        def collectFunctionRet(fs: FunctionSymbol, t: Type) = {
            val newData = functions.get(fs.name) match {
                case Some(data) =>
                    (data._1, t)
                case None =>
                    (Nil, t)
            }

            functions += (fs.name -> newData)

        }
        def collectFunction(fs: FunctionSymbol, ft: TFunction) = {
            val newData = functions.get(fs.name) match {
                case Some(data) =>
                    (ft :: data._1, data._2)
                case None =>
                    (ft :: Nil, TAny)
            }

            functions += (fs.name -> newData)
        }
    }

    class TypeEnvironment(val map: Map[CFGSimpleVariable, Type], val scope: Option[ClassSymbol], val store: ObjectStore) extends Environment[TypeEnvironment, CFGStatement] {
        def this(scope: Option[ClassSymbol]) = {
            this(new HashMap[CFGSimpleVariable, Type], scope, new ObjectStore);
        }

        def this() = {
            this(new HashMap[CFGSimpleVariable, Type], None, new ObjectStore);
        }

        def lookup(v: CFGSimpleVariable): Option[Type] = map.get(v)

        def inject(v: CFGSimpleVariable, typ: Type): TypeEnvironment =
            new TypeEnvironment(map + ((v, typ)), scope, store)

        def setStore(st: ObjectStore): TypeEnvironment = {
            new TypeEnvironment(map, scope, st)
        }

        def setObject(id: ObjectId, ot: TRealObject): TypeEnvironment = {
            new TypeEnvironment(map, scope, store.set(id, ot))
        }

        def initObjectIfNotExist(id: ObjectId, cl: Option[ClassSymbol]) = {
            new TypeEnvironment(map, scope, store.initIfNotExist(id, cl))
        }

        def copy: TypeEnvironment =
            new TypeEnvironment(Map[CFGSimpleVariable, Type]()++map, scope, store)

        def union(e: TypeEnvironment): TypeEnvironment = {
            e match {
                case BaseTypeEnvironment =>
                    this

                case te: TypeEnvironment =>
                    var newmap = Map[CFGSimpleVariable, Type]();

                    for (k <- map.keySet ++ e.map.keySet) {
                        newmap = newmap.update(k,
                            map.getOrElse(k, TUninitialized) union e.map.getOrElse(k, TUninitialized))
                    }

                    new TypeEnvironment(newmap, scope, te.store union store)
            }
        }

        def checkMonotonicity(e: TypeEnvironment, inEdges: Iterable[(CFGStatement, TypeEnvironment)]): Unit = {
            var delim = false;
            for ((v, t) <- map) {
                if (e.map contains v) {
                    if (!TypeLattice.leq(this, e, t, e.map(v))) {
                        if (!delim) {
                            println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                            println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                            delim = true;
                        }
                        println(" "+v+" => ")
                        println("      OLD: "+t)
                        println("      NEW: "+e.map(v))
                        println(" incoming values: ")
                        for ((cfg, e) <- inEdges) {
                            println("   * "+cfg+" => "+e.lookup(v))
                            println
                        }
                        println("@@@@@@@@@@@@@@@@@")
                    }
                } else {
                    if (!delim) {
                        println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                        println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                        delim = true;
                    }
                    println(" "+v+" is not in NEW ?")
                }
            }
        }

        override def equals(e: Any): Boolean = {
            e match {
                case BaseTypeEnvironment =>
                    false

                case env: TypeEnvironment =>
                    scope == env.scope && map == env.map && store == env.store
                case _ =>
                    false

            }
        }

        override def toString = {
            def typeToString(t: Type): String = t match {
                case or: TObjectRef =>
                    "(#"+or.id.pos+","+or.id.offset+")"+store.lookup(or).toString
                case _ => t.toString
            }
            
            map.toList.filter( tmp => tmp._1.toString.toList.head != '_' && tmp._1.toString != "GLOBALS").sort{(x,y) => x._1.uniqueID < x._1.uniqueID}.map(x => x._1+" => "+typeToString(x._2)).mkString("[ ", "; ", " ]");
        }
    }

    case class TypeTransferFunction(silent: Boolean, collectAnnotations: Boolean) extends TransferFunction[TypeEnvironment, CFGStatement] {
        //def notice(msg: String, pos: Positional) = if (!silent) { new Exception(msg).printStackTrace(); Reporter.notice(msg, pos) }
        //def error(msg: String, pos: Positional) = if (!silent) { new Exception(msg).printStackTrace(); Reporter.error(msg, pos) }
        def notice(msg: String, pos: Positional) = if (!silent) Reporter.notice(msg, pos)
        def error(msg: String, pos: Positional) = if (!silent) Reporter.error(msg, pos)

        def containsUninit(t: Type) = t match {
            case TTop =>
                true
            case TUninitialized =>
                true
            case tu: TUnion =>
                tu.types contains TUninitialized
            case _ =>
                false;
        }

        def removeUninit(t: Type): Type = t match {
            case TTop =>
                TAny
            case TUninitialized =>
                TBottom
            case tu: TUnion =>
                tu.types.map { removeUninit } reduceLeft (_ union _)
            case ta: TArray =>
                new TArray(Map[String, Type]() ++ ta.entries.map{ e => (e._1, removeUninit(e._2)) }, removeUninit(ta.globalType))
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
            case ta: TArray =>
                new TArray(Map[String, Type]() ++ ta.entries.map{ e => (e._1, uninitToNull(e._2)) }, uninitToNull(ta.globalType))
            case _ =>
                t
        }

        def apply(node : CFGStatement, envInit : TypeEnvironment) : TypeEnvironment = {
            var env = envInit

            def typeFromSV(sv: CFGSimpleValue): Type =
                typeFromSVR(sv, true)

            def typeFromSVR(sv: CFGSimpleValue, refine: Boolean): Type = sv match {
                case CFGLong(value)         => TInt
                case CFGFloat(value)        => TFloat
                case CFGString(value)       => TString
                case CFGTrue()              => TTrue
                case CFGFalse()             => TFalse
                case CFGAny()               => TAny
                case CFGNone()              => TBottom
                case CFGNull()              => TNull
                case CFGThis()              => getObject(node, env.scope)
                case CFGEmptyArray()        => new TArray()
                case CFGInstanceof(lhs, cl) => TBoolean
                case CFGArrayNext(ar)       => typeFromSVR(ar, refine)
                case CFGArrayCurElement(id: CFGSimpleVariable) =>
                    env.lookup(id) match {
                        case Some(TAnyArray) =>
                            TAny
                        case Some(t: TArray) =>
                            val et = if (t.entries.size > 0) {
                                t.entries.values.reduceLeft(_ join _)
                            } else {
                                TBottom
                            }

                            et union removeUninit(t.globalType)
                        case _ =>
                            TAny
                    }
                case CFGArrayCurElement(ar) => TAny
                case CFGArrayCurKey(ar)     => TString union TInt
                case CFGArrayCurIsValid(ar) =>
                    expGeneric(ar, refine, TAnyArray)
                    TBoolean
                case CFGNew(cr, params) => cr match {
                    case parser.Trees.StaticClassRef(_, _, id) =>
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
                case cl @ CFGClone(obj) =>
                    expGeneric(obj, refine, TAnyObject) match {
                        case ref: TObjectRef =>
                            val ro = env.store.lookup(ref)
                            env = env.setStore(env.store.set(ObjectId(cl.uniqueID, 0), ro))
                            new TObjectRef(ObjectId(cl.uniqueID, 0))
                        case _ =>
                            TAnyObject
                    }
                case fcall @ CFGFunctionCall(id, args) =>
                    GlobalSymbols.lookupFunction(id.value) match {
                        case Some(fs) =>
                                if (collectAnnotations) {
                                    val ft = new TFunction(args.map(a => (typeFromSVR(a, refine), false)), TBottom)
                                    AnnotationsStore.collectFunction(fs, ft);
                                }
                                checkFCalls(fcall.params, List(functionSymbolToFunctionType(fs)), fcall)
                        case None =>
                            // handle special functions
                            id.value.toLowerCase match {
                                case "isset" | "empty" =>
                                    TBoolean // no need to check the args, this is a no-error function
                                case _ =>
                                    notice("Function "+id.value+" appears to be undefined!", id)
                                    TBottom
                            }
                    }
                case mcall @ CFGMethodCall(r, mid, args) =>
                    expGeneric(r, refine, TAnyObject) match {
                        case (or: TObjectRef) =>
                            val ro = env.store.lookup(or);
                            ro.lookupMethod(mid.value, env.scope) match {
                                case Some(mt) =>
                                    if (collectAnnotations) {
                                        // Create a FunctionType and add it to the list of potential prototypes
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
                            TBottom
                    }

                case const @ CFGConstant(id) =>
                    GlobalSymbols.lookupConstant(id.value) match {
                        case Some(cs) =>
                            cs.typ
                        case None =>
                            if (Main.verbosity > 0) {
                                notice("Undefined constant '" + id.value + "'", const)
                            }
                            TString
                    }

                case const @ CFGClassConstant(cl, id) =>
                    TAny // TODO

                case const @ CFGClassProperty(cl, index) =>
                    TAny // TODO

                case mcall @ CFGStaticMethodCall(cl, id, args) =>
                    TAny // TODO

                case tern @ CFGTernary(iff, then, elze) =>
                    typeFromSVR(then, refine) union typeFromSVR(elze, refine)

                case CFGCast(typ, v) =>
                    /*
                     * TODO: not all cast from-to types are accepted, we could
                     * check for those!
                     */
                    import parser.Trees._
                    typ match {
                        case CastUnset => TNull
                        case CastInt => TInt
                        case CastString => TString
                        case CastDouble => TFloat
                        case CastArray => TAnyArray
                        case CastBool => TBoolean
                        case CastObject => TAnyObject
                    }

                case id: CFGSimpleVariable =>
                  env.lookup(id).getOrElse(TUninitialized)

                case CFGArrayEntry(ar, ind) =>
                    expGeneric(ind, refine, TString, TInt)
                    expGeneric(ar, refine, TAnyArray) match {
                        case t: TArray =>
                            t.lookup(ind)
                        case TBottom =>
                            TBottom
                        case t =>
                            println("Woops?? invlid type returned from expect: "+t);
                            TBottom
                    }

                case op @ CFGObjectProperty(obj, p) =>
                    expGeneric(p, refine, TString)
                    expGeneric(obj, refine, TAnyObject) match {
                        case TAnyObject =>
                            TAny
                        case or: TObjectRef =>
                            env.store.lookup(or).lookupField(p)
                        case TBottom =>
                            TBottom
                        case u: TUnion =>
                            u.types.map { _ match {
                                case TAnyObject =>
                                    TAny
                                case or: TObjectRef =>
                                    env.store.lookup(or).lookupField(p)
                                case _ =>
                                    TBottom
                            }}.reduceLeft(_ union _)
                        case t =>
                            println("Woops?? invlid type returned from expect: "+t);
                            TAny
                    }
                case CFGNextArrayEntry(arr) =>
                    expGeneric(arr, refine, TAnyArray) match {
                        case t: TArray =>
                            t.globalType
                        case TBottom =>
                            TBottom
                        case _ =>
                            println("woot! this is inconsistent!")
                            TAny
                   }

                case vv @ CFGVariableVar(v) =>
                    notice("Dynamic variable ignored", vv)
                    expGeneric(v, refine, TString);

                case u =>
                  println("Unknown simple value: "+u)
                  TAny
            }

            def getObject(node: CFGStatement, ocs: Option[ClassSymbol]): ObjectType = {
                val id = ObjectId(node.uniqueID, 0);
                env = env.setStore(env.store.initIfNotExist(id, ocs))
                new TObjectRef(id)
            }

            def refineType(vtyp: Type, rtyp: Type): Type =
                refineTypeDepthLimit(vtyp, rtyp, -1)

            def refineTypeDepthLimit(vtyp: Type, rtyp: Type, limit: Int): Type = {
                println("Refining type "+vtyp+" to "+rtyp +" with limit "+limit)
                def mergeArrays(svta: TArray, ta: TArray): Type = {
                    println("Merging "+svta+" with "+ta)
                    var newEntries = Map[String, Type]();

                    /* All entries that are in svta and not in ta will
                     * get merged with the globaltype of ta, - TUninit.
                     * => an existing array entry cannot be set to Uninit by
                     * a $arr[].
                     */
                    val cleantagt = removeUninit(ta.globalType)

                    for ((k, oldt) <- svta.entries) {
                        newEntries = newEntries + (k -> (oldt union cleantagt))
                    }

                    /*
                     * All entries that are in both will get refined pointwise
                     * All entries that are only in refined type will get imported
                     * as such
                     */
                    for ((k, newt) <- ta.entries) newEntries.get(k) match {
                        case Some(oldt) =>
                            newEntries = newEntries + (k -> refineTypeDepthLimit(oldt, newt, limit-1))
                        case None =>
                            newEntries = newEntries + (k -> newt)
                    }

                    /*
                     * Special refinement for the global type:
                     * If we're on the last step of merging, we use the new global type
                     * Otherwize, we merge the global types
                     */
                    val res = if (limit-1 == 0) {
                        new TArray(newEntries, ta.globalType)
                    } else {
                        new TArray(newEntries, svta.globalType union ta.globalType)
                    }
                    println("Result: "+res)
                    res
                }

                if (limit == 0) {
                    rtyp
                } else { 
                    val res = (vtyp, rtyp) match {
                        case (svta: TArray, ta: TArray) =>
                            /*
                             * If we have two arrays, we merge them specially
                             */
                            mergeArrays(svta, ta)
                        case (svtu: TUnion, ta: TArray) =>
                            /*
                             * If we know from the TUnion that it _could_ be an array
                             * we use that info as well.
                             */
                            val tua = svtu.types.find(x => x.isInstanceOf[TArray])
                            tua match {
                                case Some(svta: TArray) =>
                                    mergeArrays(svta, ta)
                                case _ =>
                                    ta
                            }
                        case (svtu: TUnion, tu: TUnion) =>
                            /*
                             * We got "expected x or y, found w or z".
                             * The idea is to compute the intersection of both unions.
                             * If empty, the best bet is the union itself
                             */
                            var resUnion = Set[Type]();

                            for (svt <- svtu.types) {
                               if (TypeLattice.leq(env, svt, tu)) {
                                   resUnion = resUnion + svt;
                               }
                            }

                            if (resUnion.size == 0) {
                                tu
                            } else if (resUnion.size == 1) {
                                resUnion.toList.head
                            } else {
                                TUnion(resUnion)
                            }
                        case (_, rt) =>
                            /*
                             * In other cases, we always use the result type
                             */
                            rt
                    }

                    //println("Result: "+res)
                    res
                }
            }

            def expNoRef(v1: CFGSimpleValue, typs: Type*): Type =
                expGeneric(v1, false, typs: _*)

            def expOrRef(v1: CFGSimpleValue, typs: Type*): Type =
                expGeneric(v1, true, typs: _*)

            def expGeneric(v1: CFGSimpleValue, refine: Boolean, typs: Type*): Type = {
                val vtyp = typeFromSVR(v1, refine)
                var vtypCheck = vtyp
                val etyp = typs reduceLeft (_ join _)

                // if verbosity is == 0, we remove Uninit from all types
                if (Main.verbosity == 0) {
                    vtypCheck = removeUninit(vtypCheck)
                }

                if (TypeLattice.leq(env, vtypCheck, etyp)) {
                    uninitToNull(vtypCheck)
                } else {
                    def errorKind(kind: String) = {
                        if (!silent) {
                            if (containsUninit(vtypCheck)) {
                                notice("Potentially undefined "+kind+": "+stringRepr(v1), v1)
                            } else {
                                if (vtypCheck != TAny || Main.verbosity > 0) {
                                    notice("Potential type mismatch: expected: "+typs.toList.map{x => x.toText(env)}.mkString(" or ")+", found: "+vtypCheck.toText(env), v1)
                                }
                            }
                        }
                    }

                    v1 match {
                        case sv: CFGSimpleVariable =>
                            errorKind("variable")
                            val refTyp = refineType(vtyp, etyp)
                            if (refine) {
                                env = env.inject(sv, refTyp)
                            }
                            refTyp

                        case v: CFGArrayEntry =>
                            errorKind("array entry")
                            val refTyp = refineType(vtyp, etyp)
                            if (refine) {
                                assign(v, refTyp)
                            }
                            refTyp

                        case v: CFGObjectProperty =>
                            errorKind("object property")
                            val refTyp = refineType(vtyp, etyp)
                            if (refine) {
                                assign(v, refTyp)
                            }
                            refTyp

                        case _ =>
                            if (!silent && (vtypCheck != TAny || Main.verbosity > 0)) {
                                notice("Potential type mismatch: expected: "+typs.toList.map{x => x.toText(env)}.mkString(" or ")+", found: "+vtypCheck.toText(env), v1)
                            }
                           etyp

                    }
                }
            }

            def typeFromUnOP(op: CFGUnaryOperator, v1: CFGSimpleValue): Type = op match {
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

            def typeFromBinOP(v1: CFGSimpleValue, op: CFGBinaryOperator, v2: CFGSimpleValue): Type = op match {
                case PLUS | MINUS | MULT | DIV  =>
                    expOrRef(v1, TInt, TFloat) match {
                        case TInt =>
                            expOrRef(v2, TInt, TFloat)
                        case TFloat =>
                            expOrRef(v2, TInt, TFloat)
                            TFloat
                        case _ =>
                            expOrRef(v2, TInt, TFloat)
                    }
                case MOD =>
                    expOrRef(v1, TInt)
                    expOrRef(v2, TInt)
                    TInt
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

            def assign(v: CFGVariable, ext: Type): Unit= {
                // we compute the type that the base variable should have

                def computeTypes(sv: CFGSimpleValue, ct: Type, rt: Type, dn: Int): (CFGSimpleVariable, Type, Type, Int) = sv match {
                    case CFGVariableVar(v) =>
                        computeTypes(v, TString, TString, dn + 1)
                    case CFGArrayEntry(arr, index) =>
                        typeFromSVR(arr, false) match {
                            case TString =>
                                // If arr is known to be a string, index must be Int
                                expOrRef(index, TInt)
                                computeTypes(arr, TString, TString, dn + 1)
                            case to: ObjectType =>
                                Predef.error("TODO: object[index] not yet implemented")
                            case _ =>
                                expOrRef(index, TString, TInt)
                                val arrayCheckType = new TArray().setAny(TTop).inject(index, ct);
                                val arrayResType   = new TArray().inject(index, rt);
                                computeTypes(arr, arrayCheckType, arrayResType, dn + 1)
                        }
                    case CFGNextArrayEntry(arr) =>
                        val arrayResType   = new TArray().injectAny(rt);
                        computeTypes(arr, TAnyArray, arrayResType, dn + 1)
                    case CFGObjectProperty(obj, prop) =>
                        // Object type used to refine
                        val objCheckRef = new TObjectRef(ObjectId(sv.uniqueID, 1));
                        env = env.initObjectIfNotExist(objCheckRef.id, None)
                        val objCheck = env.store.lookup(objCheckRef).setAnyField(TTop).injectField(prop, ct, false)
                        env = env.setObject(objCheckRef.id, objCheck)

                        // Object type used to check
                        val objRefRef = new TObjectRef(ObjectId(sv.uniqueID, 0));
                        env = env.initObjectIfNotExist(objRefRef.id, None)
                        val objRef = env.store.lookup(objRefRef).injectField(prop, rt)
                        env = env.setObject(objRefRef.id, objRef)

                        computeTypes(obj, objCheckRef, objRefRef, dn + 1)
                    case svar: CFGSimpleVariable =>
                        (svar, ct, rt, dn)
                    case _ =>
                        Predef.error("Woops, unexpected CFGVariable inside checktype of!")

                }

                val (svar, ct, rt, depth) = v match {
                    case CFGArrayEntry(arr, index) =>
                        typeFromSVR(arr, false) match {
                            case TString =>
                                expOrRef(index, TInt)
                                computeTypes(arr, TString, TString, 1)
                            case to: ObjectType =>
                                Predef.error("TODO: object[index] not yet implemented")
                            case _ =>
                                expOrRef(index, TInt, TString)
                                val arrayResType   = new TArray().inject(index, ext);
                                computeTypes(arr, TAnyArray, arrayResType, 1)
                        }
                    case CFGNextArrayEntry(arr) =>
                        val arrayResType   = new TArray().injectAny(ext);
                        computeTypes(arr, TAnyArray, arrayResType, 1)
                    case sv: CFGSimpleVariable =>
                        (sv, TTop, ext, 0)
                    case CFGVariableVar(v) =>
                        computeTypes(v, TString, TString, 0)
                    case CFGObjectProperty(obj, prop) =>
                        val objRefType = new TObjectRef(ObjectId(v.uniqueID, 0));
                        env = env.setStore(env.store.initIfNotExist(objRefType.id, None))
                        env = env.setStore(env.store.set(objRefType.id, env.store.lookup(objRefType).injectField(prop, ext)))
                        computeTypes(obj, TAnyObject, objRefType, 0)
                    case _ =>
                        Predef.error("Woot, not complex? "+v)
                }
                // We can check for that type, without refining it with ct
                println("Checking "+svar+" against "+ct)
                expNoRef(svar, ct)
                // We now need to refine the type with rt
                env = env.inject(svar, refineTypeDepthLimit(typeFromSVR(svar, false), rt, depth))
            }

            def functionSymbolToFunctionType(fs: FunctionSymbol): FunctionType = {
                new TFunction(fs.argList.map { a => (a._2.typ, a._2.optional) }, fs.typ)
            }

            def checkFCalls(fcall_params: List[CFGSimpleValue], syms: List[FunctionType], pos: Positional) : Type =  {
                def protoFilter(sym: FunctionType): Boolean = {
                    sym match {
                        case tf: TFunction =>
                            var ret = true;
                            for (i <- fcall_params.indices) {
                                if (i >= tf.args.length) {
                                    ret = false
                                } else {
                                    if (!TypeLattice.leq(env, typeFromSV(fcall_params(i)), tf.args(i)._1)) {
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
                            syms.first match {
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
                case CFGAssign(vr: CFGVariable, v1) =>
                    val t = typeFromSV(v1)
                    assign(vr, t)

                case CFGAssignUnary(vr: CFGVariable, op, v1) =>
                    // We want to typecheck v1 according to OP
                    val t = typeFromUnOP(op, v1);
                    assign(vr, t)

                case CFGAssignBinary(vr: CFGVariable, v1, op, v2) =>
                    // We want to typecheck v1/v2 according to OP
                    val t = typeFromBinOP(v1, op, v2)
                    assign(vr, t)

                case CFGAssume(v1, op, v2) => op match {
                    case LT | LEQ | GEQ | GT =>
                        expOrRef(v1, TInt, TFloat)
                        expOrRef(v2, TInt, TFloat)
                    case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                        /**
                         * Type filtering:
                         * if v1 is a variable that is compared against True/False
                         * we can filter incompatible values out
                         */
                        def filter(v: CFGSimpleVariable, value: Boolean) = {
                            typeFromSV(v) match {
                                case u: TUnion =>
                                    val typ = if (value) {
                                        u.types.filter(t => t != TFalse && t != TNull).map(t => if(t == TBoolean) TTrue else t).reduceLeft(_ union _)
                                    } else {
                                        u.types.filter(t => t != TTrue  && t != TResource).map(t => if(t == TBoolean) TFalse else t).reduceLeft(_ union _)
                                    }

                                    env = env.inject(v, typ)
                                case t =>
                                    if (value) {
                                        if (t != TFalse && t != TNull) {
                                            env = env.inject(v, t)
                                        } else {
                                            // we had a single incompatible type
                                            // The branch will never be taken!
                                            env = BaseTypeEnvironment
                                        }
                                    } else {
                                        if (t != TTrue && t != TResource) {
                                            env = env.inject(v, t)
                                        } else {
                                            // we had a single incompatible type
                                            // The branch will never be taken!
                                            env = BaseTypeEnvironment
                                        }
                                    }
                            }
                        }
                        var t1 = v1;
                        var t2 = v2;
                        (v1, typeFromSV(v1), v2, typeFromSV(v2)) match {
                            case (v: CFGSimpleVariable, TTrue , w: CFGSimpleVariable, _) =>
                                // we benefit from a switch:
                                t1 = v2;
                                t2 = v1;
                            case (v: CFGSimpleVariable, TFalse , w: CFGSimpleVariable, _) =>
                                // we benefit from a switch:
                                t1 = v2;
                                t2 = v1;
                            case (v: CFGSimpleVariable, _, _, _) =>
                                // no change
                            case (_, _, v: CFGSimpleVariable, _) =>
                                t1 = v2;
                                t2 = v1;
                            case _ =>
                                // no change, will be ignored anyway
                        }
                        (t1, op, typeFromSV(t2)) match  {
                            case (v: CFGSimpleVariable, EQUALS, TFalse)  =>
                                filter(v, false)
                            case (v: CFGSimpleVariable, NOTEQUALS, TFalse)  =>
                                filter(v, true)
                            case (v: CFGSimpleVariable, EQUALS, TTrue)  =>
                                filter(v, true)
                            case (v: CFGSimpleVariable, NOTEQUALS, TTrue)  =>
                                filter(v, false)
                            case _ =>
                                expOrRef(v1, TAny)
                                expOrRef(v2, TAny)
                        }

                  }

                case CFGPrint(v) =>
                    expOrRef(v, TInt, TString, TAnyObject, TBoolean)

                case CFGUnset(id) =>
                    id match {
                        case v: CFGSimpleVariable =>
                            env = env.inject(v, TUninitialized)
                        case _ =>
                            // TODO

                    }

                case ex: CFGSimpleValue =>
                    expOrRef(ex, TAny)

                case _ => notice(node+" not yet handled", node); env
            }

            env
        }
    }

    case class Analyzer(cfg: CFG, scope: Scope) {

        def setupEnvironment: TypeEnvironment = {
            var baseEnv   = new TypeEnvironment;

            // We now inject predefined variables
            def injectPredef(name: String, typ: Type): Unit = {
                scope.lookupVariable(name) match {
                    case Some(vs) =>
                        baseEnv = baseEnv.inject(CFGIdentifier(vs), typ)
                    case None =>
                        // ignore this var
                        println("Woops, no such symbol found: "+name)
                }
            }

            //scope.registerPredefVariables
            injectPredef("_GET",     new TArray(TBottom))
            injectPredef("_POST",    new TArray(TBottom))
            injectPredef("GLOBALS",  new TArray(TBottom))
            injectPredef("_REQUEST", new TArray(TBottom))
            injectPredef("_COOKIE",  new TArray(TBottom))
            injectPredef("_SERVER",  new TArray(TBottom))
            injectPredef("_ENV",     new TArray(TBottom))
            injectPredef("_SESSION", new TArray(TBottom))

            // for methods, we inject $this as its always defined
            scope match {
                case ms: MethodSymbol =>
                    baseEnv = baseEnv.setStore(baseEnv.store.initIfNotExist(ObjectId(-1, 0), Some(ms.cs)))
                    injectPredef("this", new TObjectRef(ObjectId(-1, 0)))
                case _ =>
            }

            // in case we have a function or method symbol, we also inject arguments
            scope match {
                case fs: FunctionSymbol =>
                    for ((name, sym) <- fs.argList) {
                        baseEnv = baseEnv.inject(CFGIdentifier(sym), sym.typ)
                    }
                case _ =>

            }

            baseEnv
        }

        def analyze: Unit = {
            val bottomEnv = BaseTypeEnvironment;
            val baseEnv   = setupEnvironment;

            val aa = new AnalysisAlgorithm[TypeEnvironment, CFGStatement](TypeTransferFunction(true, false), bottomEnv, baseEnv, cfg)

            aa.init
            aa.computeFixpoint

            if (Main.displayFixPoint) {
                println("     - Fixpoint:");
                for ((v,e) <- aa.getResult.toList.sort{(x,y) => x._1.name < y._1.name}) {
                    println("      * ["+v+"] => "+e);
                }
            }

            // Detect unreachables:
            for (l <- aa.detectUnreachable(TypeTransferFunction(true, false))) {
                Reporter.notice("Unreachable code", l)
            }

            // Collect errors and annotations
            aa.pass(TypeTransferFunction(false, !Main.exportAPIPath.isEmpty))

            // Collect retvals
            scope match {
                case fs: FunctionSymbol =>
                    // collect return value
                    val facts = aa.getResult;
                    val retType = facts(cfg.exit).map.getOrElse(CFGTempID("retval"), TBottom);

                    AnnotationsStore.collectFunctionRet(fs, retType)
                case _ =>
            }
        }
    }
}
