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
                    case (r1: TRealClassObject, r2: TRealObject) =>
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
            case (TAny, _: ConcreteType) => TAny
            case (_: ConcreteType, TAny) => TAny

            case (TTrue, TFalse) => TBoolean
            case (TFalse, TTrue) => TBoolean
            case (TBottom, _) => y
            case (_, TBottom) => x

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

    class TypeEnvironment(val map: Map[CFGSimpleVariable, Type], val scope: Option[ClassSymbol], val store: ObjectStore) extends Environment[TypeEnvironment] {
        def this(scope: Option[ClassSymbol]) = {
            this(new HashMap[CFGSimpleVariable, Type], scope, new ObjectStore);
        }

        def this() = {
            this(new HashMap[CFGSimpleVariable, Type], None, new ObjectStore);
        }

        def lookup(v: CFGSimpleVariable): Option[Type] = map.get(v)

        def inject(v: CFGSimpleVariable, typ: Type): TypeEnvironment =
            new TypeEnvironment(map + ((v, typ)), scope, store)

        def injectStore(st: ObjectStore): TypeEnvironment =
            new TypeEnvironment(map, scope, st)

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

        def checkMonotonicity(e: TypeEnvironment): Boolean = {
            map.forall { x =>
                if (e.map contains x._1) {
                    if (!TypeLattice.leq(this, e, x._2, e.map(x._1))) {
                        false
                    } else {
                        true
                    }
                } else {
                    false
                }
            }
        }

        def dumpDiff(e: TypeEnvironment): Unit = {
            if (scope != e.scope) {
                println("Scope diff!")
            } else if (store != e.store) {
                println("Store diff!")
            } else {
                for ((v, t) <- map) {
                    if (e.map contains v) {
                        if (t != e.map(v)) {
                            println(" "+v+" => ")
                            println("      OLD: "+t)
                            println("      NEW: "+e.map(v))
                        }
                    } else {
                        println(" "+v+" not in NEW:"+t)
                    }
                }
                for ((v, t) <- e.map) {
                    if (!(map contains v)) {
                        println(" "+v+" not in OLD: "+t)
                    }
                }
            }
        }

        override def equals(e: Any): Boolean = {
            e match {
                case BaseTypeEnvironment =>
                    false

                case env: TypeEnvironment =>

                    /*
                    if (scope != env.scope) {
                        println("Scope mismatch:")
                        println(" - "+scope)
                        println(" - "+env.scope)
                    }

                    if (map != env.map) {
                        println("Map mismatch:")
                        println(" - "+map)
                        println(" - "+env.map)
                    }


                    if (store != env.store) {
                        println("Store mismatch:")
                        println(" - "+store)
                        println(" - "+env.store)
                    }
                    */

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
            
            map.toList.filter(_._1.toString.toList.head != '_').sort{(x,y) => x._1.uniqueID < x._1.uniqueID}.map(x => x._1+" => "+typeToString(x._2)).mkString("[ ", "; ", " ]");
        }
    }

    case class TypeTransferFunction(silent: Boolean) extends TransferFunction[TypeEnvironment, CFGStatement] {
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

        def uninitToNull(t: Type): Type = t match {
            case TTop =>
                TAny
            case TUninitialized =>
                TNull
            case tu: TUnion =>
                tu.types.map { x => if (x == TUninitialized) TNull else x } reduceLeft (_ union _)
            case _ =>
                t
        }

        def apply(node : CFGStatement, env : TypeEnvironment) : TypeEnvironment = {
            var store = env.store

            def typeFromSV(env: TypeEnvironment, sv: CFGSimpleValue): (TypeEnvironment, Type) = sv match {
                case CFGLong(value) => (env, TInt)
                case CFGFloat(value) => (env, TFloat)
                case CFGString(value) => (env, TString)
                case CFGTrue() => (env, TTrue)
                case CFGFalse() => (env, TFalse)
                case CFGAny() => (env, TAny)
                case CFGNone() => (env, TBottom)
                case CFGNull() => (env, TNull)
                case CFGThis() => (env, getObject(node, env.scope))
                case CFGEmptyArray() => (env, new TArray())
                case CFGInstanceof(lhs, cl) => (env, TBoolean)
                case CFGArrayNext(ar) => typeFromSV(env, ar)
                case CFGArrayCurElement(id: CFGSimpleVariable) =>
                    (env, env.lookup(id) match {
                        case Some(TAnyArray) =>
                            TAny
                        case Some(t: TArray) =>
                            val et = if (t.entries.size > 0) {
                                t.entries.values.reduceLeft(_ join _)
                            } else {
                                TBottom
                            }

                            et union t.globalType
                        case _ =>
                            TAny
                    })
                case CFGArrayCurElement(ar) => (env, TAny)
                case CFGArrayCurKey(ar) => (env, TString union TInt)
                case CFGArrayCurIsValid(ar) =>
                    (expOrRef(env, ar, TAnyArray)._1, TBoolean)
                case CFGNew(cr, params) => cr match {
                    case parser.Trees.StaticClassRef(_, _, id) =>
                        (env, GlobalSymbols.lookupClass(id.value) match {
                            case a @ Some(cs) =>
                                getObject(node, a)
                            case _ =>
                                error("Undefined class '"+id.value+"'", id)
                                getObject(node, None)
                        })
                    case _ =>
                        (env, getObject(node, None))
                }
                case cl @ CFGClone(obj) =>
                    expOrRef(env, obj, TAnyObject) match {
                        case (env, ref: TObjectRef) =>
                            val ro = store.lookup(ref)
                            store = store.set(ObjectId(cl.uniqueID, 0), ro)
                            (env, new TObjectRef(ObjectId(cl.uniqueID, 0)))
                        case (env, _)=>
                            (env, TAnyObject)
                    }
                case fcall @ CFGFunctionCall(id, args) =>
                    GlobalSymbols.lookupFunction(id.value) match {
                        case Some(fs) =>
                                checkFCalls(env, fcall.params, List(functionSymbolToFunctionType(fs)), fcall)
                        case None =>
                            // handle special functions
                            val typ = id.value.toLowerCase match {
                                case "isset" | "empty" =>
                                    TBoolean // no need to check the args, this is a no-error function
                                case _ =>
                                    notice("Function "+id.value+" appears to be undefined!", id)
                                    TBottom
                            }

                            (env, typ)
                    }
                case mcall @ CFGMethodCall(r, mid, args) =>
                    expOrRef(env, r, TAnyObject) match {
                        case (env, or: TObjectRef) =>
                            val ro = store.lookup(or);
                            ro.lookupMethod(mid.value, env.scope) match {
                                case Some(mt) =>
                                    checkFCalls(env, args, List(mt), mcall)
                                case None =>
                                    // Check for magic __call ?
                                    val cms = ro.lookupMethod("__call", env.scope)
                                    val t = if (cms == None) {
                                        notice("Undefined method '" + mid.value + "' in object "+ro, mid)
                                        TBottom
                                    } else {
                                        cms.get.ret
                                    }

                                    (env, t)
                            }
                        case _ =>
                            (env, TBottom)
                    }

                case const @ CFGConstant(id) =>
                    (env, GlobalSymbols.lookupConstant(id.value) match {
                        case Some(cs) =>
                            cs.typ
                        case None =>
                            notice("Undefined constant '" + id.value + "'", const)
                            TString
                    })

                case const @ CFGClassConstant(cl, id) =>
                    (env, TAny) // TODO

                case const @ CFGClassProperty(cl, index) =>
                    (env, TAny) // TODO

                case mcall @ CFGStaticMethodCall(cl, id, args) =>
                    (env, TAny) // TODO

                case tern @ CFGTernary(iff, then, elze) =>
                    val t1 = typeFromSV(env, then);
                    val t2 = typeFromSV(env, elze);

                    (t1._1 union t2._1, t1._2 union t2._2)

                case CFGCast(typ, v) =>
                    // TODO: not all cast from-to types are accepted, we could
                    // check for those!
                    import parser.Trees._
                    (env, typ match {
                        case CastUnset => TNull
                        case CastInt => TInt
                        case CastString => TString
                        case CastDouble => TFloat
                        case CastArray => TAnyArray
                        case CastBool => TBoolean
                        case CastObject => TAnyObject
                    })

                case id: CFGSimpleVariable =>
                  (env, env.lookup(id) match {
                    case Some(t) =>
                        t
                    case None =>
                        TUninitialized
                  })

                case CFGArrayEntry(ar, ind) =>
                    expOrRef(expOrRef(env, ind, TString, TInt)._1, ar, TAnyArray) match {
                        case (env, t: TArray) =>
                            (env, t.lookup(ind))
                        case (env, TBottom) =>
                            (env, TBottom)
                        case (env, t) =>
                            println("Woops?? invlid type returned from expect: "+t);
                            (env, TBottom)
                    }

                case op @ CFGObjectProperty(obj, p) =>
                    expOrRef(expOrRef(env, p, TString)._1, obj, TAnyObject) match {
                        case (env, TAnyObject) =>
                            (env, TAny)
                        case (env, or: TObjectRef) =>
                            (env, store.lookup(or).lookupField(p))
                        case (env, TBottom) =>
                            (env, TBottom)
                        case (env, u: TUnion) =>
                            (env, TUnion(u.types.map { _ match {
                                case TAnyObject =>
                                    TAny
                                case or: TObjectRef =>
                                    store.lookup(or).lookupField(p)
                                case _ =>
                                    TBottom
                            }}))
                        case (env, t) =>
                            println("Woops?? invlid type returned from expect: "+t);
                            (env, TAny)
                    }
                case CFGNextArrayEntry(arr) =>
                    typeFromSV(env, arr) match {
                        case (env, t: TArray) =>
                            (env, t.globalType)
                        case _ =>
                            println("woot! this is inconsistent!")
                            (env, TAny)
                    }

                case vv @ CFGVariableVar(v) =>
                    notice("Dynamic variable ignored", vv)
                    expOrRef(env, v, TString);

                case u =>
                  println("Unknown simple value: "+u)
                  (env, TAny)
            }

            def getObject(node: CFGStatement, ocs: Option[ClassSymbol]): ObjectType = {
                val id = ObjectId(node.uniqueID, 0);
                store = store.initIfNotExist(id, ocs)
                new TObjectRef(id)
            }

            def expOrRef(env_init: TypeEnvironment, v1: CFGSimpleValue, typs: Type*): (TypeEnvironment, Type) = {
                val tmp  = typeFromSV(env_init, v1);
                var env  = tmp._1;
                val vtyp = tmp._2;
                val etyp = typs reduceLeft (_ join _)

                if (TypeLattice.leq(env, vtyp, etyp)) {
                    (env, uninitToNull(vtyp))
                } else {
                    def error(kind: String) = {
                        if (!silent) {
                            if (containsUninit(vtyp)) {
                                notice("Potentially undefined "+kind+": "+stringRepr(v1), v1)
                            } else {
                                notice("Potential type mismatch: expected: "+typs.toList.map{x => x.toText(env)}.mkString(" or ")+", found: "+vtyp.toText(env), v1)
                            }
                        }
                    }
                    v1 match {
                        case sv: CFGSimpleVariable =>
                            error("variable")
                            (env.inject(sv, etyp), typs.toList.head)

                        case v: CFGArrayEntry =>
                            error("array entry")
                            // refine
                            (complexAssign(env, v, etyp), typs.toList.head)

                        case v: CFGObjectProperty =>
                            error("object property")
                            (complexAssign(env, v, etyp), typs.toList.head)

                        case _ =>
                            if (!silent) {
                                notice("Potential type mismatch: expected: "+typs.toList.map{x => x.toText(env)}.mkString(" or ")+", found: "+vtyp.toText(env), v1)
                            }
                            (env, typs.toList.head)

                    }
                }
            }

            def typeCheckUnOP(env: TypeEnvironment, vr: Option[CFGSimpleVariable], op: CFGUnaryOperator, v1: CFGSimpleValue): (TypeEnvironment, Type) = {
                op match {
                    case BOOLEANNOT =>
                        expOrRef(env, v1, TAny)
                    case BITSIWENOT =>
                        expOrRef(env, v1, TInt)
                    case PREINC =>
                        expOrRef(env, v1, TInt)
                    case POSTINC =>
                        expOrRef(env, v1, TInt)
                    case PREDEC =>
                        expOrRef(env, v1, TInt)
                    case POSTDEC =>
                        expOrRef(env, v1, TInt)
                    case SILENCE =>
                        expOrRef(env, v1, TAny)
                }
            }

            def typeCheckBinOP(env: TypeEnvironment, vr: Option[CFGSimpleVariable], v1: CFGSimpleValue, op: CFGBinaryOperator, v2: CFGSimpleValue): (TypeEnvironment, Type) = {
                val tmp = op match {
                    case PLUS =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case MINUS =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case MULT =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case DIV =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case CONCAT =>
                        expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)
                    case MOD =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case INSTANCEOF =>
                        (expOrRef(expOrRef(env, v1, TAnyObject)._1, v2, TString)._1, TBoolean)
                    case BOOLEANAND =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case BOOLEANOR =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case BOOLEANXOR =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case BITWISEAND =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case BITWISEOR =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case BITWISEXOR =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case SHIFTLEFT =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case SHIFTRIGHT =>
                        expOrRef(expOrRef(env, v1, TInt)._1, v2, TInt)
                    case LT =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case LEQ =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case GEQ =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case GT =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case EQUALS =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case IDENTICAL =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case NOTEQUALS =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                    case NOTIDENTICAL =>
                        (expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1, TBoolean)
                }

                vr match {
                    case Some(vr) =>
                        (tmp._1.inject(vr, tmp._2), tmp._2)
                    case None =>
                        tmp
                }
            }

            def complexAssign(env: TypeEnvironment, v: CFGVariable, ext: Type): TypeEnvironment = {
                    var elems: List[(CFGSimpleValue, Type, Type)] = Nil;

                    def linearize(sv: CFGSimpleValue, checkType: Type, resultType: Type, pass: Int): Unit = {
                        // Recursive function that pushes each array parts and compute types
                        elems = (sv, checkType, resultType) :: elems;
                        sv match {
                            case CFGVariableVar(v) =>
                                linearize(v, TString, TString, pass+1)
                            case CFGArrayEntry(arr, index) =>
                                // We always will end up with a precise array
                                // The check type depends on the pass (i.e. deepness)
                                // pass == 0 means this is the most outer assign,
                                // which only needs to be checked against AnyArray
                                val rt = new TArray().inject(index, resultType);
                                val ct = if (pass > 0) new TArray().inject(index, checkType) else TAnyArray;

                                linearize(arr, ct, rt, pass+1)
                            case CFGObjectProperty(obj, index) =>
                                val rt = new TObjectRef(ObjectId(sv.uniqueID, 0));
                                store = store.initIfNotExist(rt.id, None)
                                store.lookup(rt).injectField(index, resultType);
                                // the check type is a different object, we create
                                // a tmp object with negative id, and shift it by
                                // the number of potentially used tmp objects used
                                // for params
                                val ct = if (pass > 0) {
                                    store = store.initIfNotExist(ObjectId(sv.uniqueID, 1), None);
                                    store.lookup(ObjectId(sv.uniqueID, 1)).injectField(index, checkType, false)
                                    new TObjectRef(ObjectId(sv.uniqueID, 1))
                                } else {
                                    TAnyObject;
                                }

                                linearize(obj, ct, rt, pass+1)
                            case CFGNextArrayEntry(arr) =>
                                // ditto ArrayEntry
                                val rt = new TArray().injectAny(resultType);
                                val ct = if (pass > 0) new TArray().injectAny(checkType) else TAnyArray;

                                linearize(arr, ct, rt, pass+1)
                            case _ =>
                        }
                    }

                    // We lineraize the recursive structure
                    linearize(v, ext, ext, 0)

                    var e = env.injectStore(store)

                    // Let's traverse all up to the last elem (the outermost assign)
                    for ((elem, ct, rt) <- elems.init) {
                        //println(" Checking for "+elem +"(actualType: "+typeFromSV(elem)+", checkType: "+ct+", resultType: "+rt+")");
                        def assignMergeObject(from: TObjectRef, to: TObjectRef): Type ={
                                val fromRO = store.lookup(from)
                                val toRO   = store.lookup(to)

                                //println("Trying to assign-merge "+fromRO+" and "+ toRO)

                                var newFields = HashMap[String, Type]() ++ fromRO.fields;

                                val pt = toRO.globalType join fromRO.globalType

                                for((index, typ) <- toRO.fields) {
                                    newFields = newFields.update(index, newFields.get(index) match {
                                        case Some(t) => pt join (t join typ) // weak assign here
                                        case None => pt join typ
                                    })
                                }

                                val o : TRealObject = toRO match {
                                    case o: TRealClassObject =>
                                        new TRealClassObject(o.cl, newFields, pt)
                                    case o: TRealObject =>
                                        new TRealObject(newFields, pt)
                                }

                                //println("Result: "+o)

                                e = e.injectStore(e.store.set(to.id, o))

                                to
                        }
                        // assignMerge will recursively merge types of recursive arrays
                        // we cannot use Lattice.Join as we do not want unions.
                        // i.e. $a['foo']['bar'] = 2; $a['foo']['bar'] =
                        // "str"; should not end with:
                        //    $a -> Array[foo => Array[bar => {TInt, TString}]]
                        // but
                        //    $a -> Array[foo => Array[bar => String]]
                        def assignMerge(from: Type, to: Type): Type = (from,to) match {
                            case (from: TArray, to: TArray) =>
                                import scala.collection.mutable.HashMap
                                import Math.max

                                val pt = to.globalType union from.globalType

                                val newEntries = Map[String, Type]() ++ from.entries;

                                for((index, typ)<- to.entries) {
                                    newEntries(index) = newEntries.get(index) match {
                                        case Some(t) => pt join assignMerge(t, typ)
                                        case None => pt join typ
                                    }
                                }

                                new TArray(newEntries, pt)

                            case (from: TObjectRef, to: TObjectRef) =>
                                assignMergeObject(from, to)
                            case (from: TObjectRef, to: TUnion) =>
                                // We may have a union of objects here
                                TUnion(to.types map { _ match {
                                    case t: TObjectRef =>
                                        assignMergeObject(from, t)
                                    case o => o
                                }})
                            case (a, b) =>
                                // In case not both types are not arrays nor objects, we
                                // always end up with the resulting type
                                a
                        }

                        val resultingType = assignMerge(rt, typeFromSV(env, elem)._2)

                        elem match {
                            case sv: CFGSimpleVariable =>
                                // Due to our deep type system, checking
                                // the base variable should be enough
                                e = expOrRef(e, elem, ct)._1
                                e = e.inject(sv, resultingType);
                            case _ =>
                        }
                    }

                  e
            }

            def functionSymbolToFunctionType(fs: FunctionSymbol): FunctionType = {
                new TFunction(fs.argList.map { a => (a._2.typ, a._2.optional) }, fs.typ)
            }

            def checkFCalls(env_init: TypeEnvironment, fcall_params: List[CFGSimpleValue], syms: List[FunctionType], pos: Positional) : (TypeEnvironment, Type) =  {
                var env = env_init;

                def protoFilter(sym: FunctionType): Boolean = {
                    sym match {
                        case tf: TFunction =>
                            var ret = true;
                            for (i <- fcall_params.indices) {
                                if (i >= tf.args.length) {
                                    ret = false
                                } else {
                                    if (!TypeLattice.leq(env, typeFromSV(env, fcall_params(i))._2, tf.args(i)._1)) {
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
                            error("Unmatched function prototype '("+fcall_params.map(x => typeFromSV(env, x)._2).mkString(", ")+")', candidates are:\n    "+syms.mkString(",\n    "), pos)
                            (env, TBottom)
                        } else {
                            val typ = syms.first match {
                                case tf: TFunction =>
                                    for (i <- fcall_params.indices) {
                                        if (i >= tf.args.length) {
                                            error("Prototype error!", pos)
                                        } else {
                                            val tmp = expOrRef(env, fcall_params(i), tf.args(i)._1)
                                            env = tmp._1
                                            tmp._2
                                        }

                                    }
                                    tf.ret
                                case s =>
                                    s.ret
                            }

                            (env, typ)
                        }

                    case f :: xs =>
                        (env, f.ret)
                }
            }


            val newEnv = node match {
                case CFGAssign(vr: CFGSimpleVariable, v1) =>
                    val tmp = typeFromSV(env, v1)
                    tmp._1.inject(vr, tmp._2)

                case CFGAssignUnary(vr: CFGSimpleVariable, op, v1) =>
                    // We want to typecheck v1 according to OP
                    typeCheckUnOP(env, Some(vr), op, v1)._1

                case CFGAssignUnary(ca: CFGVariable, op, v1) =>
                    // We want to typecheck v1 according to OP
                    val tmp = typeCheckUnOP(env, None, op, v1)
                    complexAssign(tmp._1, ca, tmp._2)

                case CFGAssignBinary(vr: CFGSimpleVariable, v1, op, v2) =>
                    // We want to typecheck v1/v2 according to OP
                    typeCheckBinOP(env, Some(vr), v1, op, v2)._1

                case CFGAssignBinary(ca: CFGVariable, v1, op, v2) =>
                    // We want to typecheck v1/v2 according to OP
                    val tmp = typeCheckBinOP(env, None, v1, op, v2)
                    complexAssign(tmp._1, ca, tmp._2)

                case CFGAssign(ca: CFGVariable, ex) =>
                    val tmp = typeFromSV(env, ex)
                    complexAssign(tmp._1, ca, tmp._2)

                case CFGAssume(v1, op, v2) => op match {
                    case LT | LEQ | GEQ | GT =>
                        expOrRef(expOrRef(env, v1, TInt, TFloat)._1, v2, TInt, TFloat)._1
                    case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                        /**
                         * Type filtering:
                         * if v1 is a variable that is compared against True/False
                         * we can filter incompatible values out
                         */
                        def filter(v: CFGSimpleVariable, value: Boolean): Type = {
                            typeFromSV(env, v)._2 match {
                                case u: TUnion =>
                                    if (value) {
                                        TUnion(u.types.filter(t => t != TFalse && t != TNull))
                                    } else {
                                        TUnion(u.types.filter(t => t != TTrue  && t != TResource))
                                    }
                                case t =>
                                    if (value) {
                                        if (t != TFalse && t != TNull) {
                                            t
                                        } else {
                                            // we had a single incompatible type
                                            // The branch will never be taken!
                                            notice("Redundant or incompatible check", v);
                                            TBottom
                                        }
                                    } else {
                                        if (t != TTrue && t != TResource) {
                                            t
                                        } else {
                                            // we had a single incompatible type
                                            // The branch will never be taken!
                                            notice("Redundant or incompatible check", v);
                                            TBottom
                                        }
                                    }
                            }
                        }
                        var t1 = v1;
                        var t2 = v2;
                        (v1, typeFromSV(env,v1)._2, v2, typeFromSV(env, v2)._2) match {
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
                        (t1, op, typeFromSV(env, t2)._2) match  {
                            case (v: CFGSimpleVariable, EQUALS, TFalse)  =>
                                env.inject(v, filter(v, false))
                            case (v: CFGSimpleVariable, NOTEQUALS, TFalse)  =>
                                env.inject(v, filter(v, true))
                            case (v: CFGSimpleVariable, EQUALS, TTrue)  =>
                                env.inject(v, filter(v, true))
                            case (v: CFGSimpleVariable, NOTEQUALS, TTrue)  =>
                                env.inject(v, filter(v, false))
                            case _ =>
                                expOrRef(expOrRef(env, v1, TAny)._1, v2, TAny)._1
                        }

                  }

                case CFGPrint(v) =>
                    expOrRef(env, v, TInt, TString, TAnyObject, TBoolean)._1

                case CFGUnset(id) =>
                    id match {
                        case v: CFGSimpleVariable =>
                            env.inject(v, TUninitialized)
                        case _ =>
                            env // TODO

                    }

                case CFGSkip =>
                    env

                case ex: CFGSimpleValue =>
                    expOrRef(env, ex, TAny)._1

                case _ => notice(node+" not yet handled", node); env
            }

            newEnv.injectStore(store)
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

            scope.registerPredefVariables
            injectPredef("_GET",     new TArray(TBottom))
            injectPredef("_POST",    new TArray(TBottom))
            injectPredef("_REQUEST", new TArray(TBottom))
            injectPredef("_COOKIE",  new TArray(TBottom))
            injectPredef("_SERVER",  new TArray(TBottom))
            injectPredef("_ENV",     new TArray(TBottom))
            injectPredef("_SESSION", new TArray(TBottom))

            // for methods, we inject $this as its always defined
            scope match {
                case ms: MethodSymbol =>
                    baseEnv = baseEnv.injectStore(baseEnv.store.initIfNotExist(ObjectId(-1, 0), Some(ms.cs)))
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

        def analyze = {
            val bottomEnv = BaseTypeEnvironment;
            val baseEnv   = setupEnvironment;

            val aa = new AnalysisAlgorithm[TypeEnvironment, CFGStatement](TypeTransferFunction(true), bottomEnv, baseEnv, cfg)

            aa.init
            aa.computeFixpoint

            if (Main.displayFixPoint) {
                println("     - Fixpoint:");
                for ((v,e) <- aa.getResult.toList.sort{(x,y) => x._1.name < y._1.name}) {
                    println("      * ["+v+"] => "+e);
                }
            }

            aa.pass(TypeTransferFunction(false))
        }
    }
}
