package phpanalysis.controlflow

import CFGTrees._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.Map
import analyzer.Symbols._
import analyzer.Types._

object TypeFlow {
    case object TypeLattice extends Lattice {
        type E = Type

        def leq(x : Type, y : Type) = (x,y) match {
            case (x, y) if x == y => true

            case (TNone, _) => true
            case (_, TAny) => true
            case (_, TBoolean) => true
            case (TFalse, TTrue) => false
            case (TNull, TTrue) => false
            case (TNull, TFalse) => true
            case (_, TTrue) => true
            case (t1: TObjectRef, TAnyObject) => true
            case (t1: TObjectRef, t2: TObjectRef) =>
                (t1.realObj, t2.realObj) match {
                    case (r1: TRealClassObject, r2: TRealClassObject) =>
                        r1.cl isSubtypeOf r2.cl
                    case (r1: RealObjectType, r2: TRealObject) =>
                        r2.fields.forall(f => r1.fields.get(f._1) != None && leq(r1.fields(f._1), f._2))
                    case _ =>
                        false
                }

            case (t1: TArray, TAnyArray) => true
            case (t1: TArray, t2: TArray) =>
                // TODO: make it more precise
                (t1.pollutedType, t2.pollutedType) match {
                    case (Some(pt1), Some(pt2)) =>
                        leq(pt1, pt2)
                    case _ =>
                        false
                }
            case (t1: TUnion, t2: TUnion) =>
                (HashSet[Type]() ++ t1.types) subsetOf (HashSet[Type]() ++ t2.types)
            case (t1, t2: TUnion) =>
                t2.types exists { x => leq(t1, x) }
            case (t1: TUnion, t2) =>
                t1.types forall { x => leq(x, t2) }
            case _ => false
        }

        val top = TAny
        val bottom = TNone

        def join(x : Type, y : Type) = {
            val res = (x,y) match {
            case (TNone, _) => y
            case (_, TNone) => x

            case (t1, t2) if t1 == t2 => t1

            // Objects
            case (TAnyObject, t: TObjectRef) => TAnyObject
            case (t: TObjectRef, TAnyObject) => TAnyObject
            case (t1: TObjectRef, t2: TObjectRef) =>
                (t1.realObj, t2.realObj) match {
                    case (tr1: TRealObject, tr2: TRealObject) =>
                        TUnion(t1, t2)
                    case _ =>
                        TAnyObject
                }

            // Arrays
            case (TAnyArray, t: TArray) => TAnyArray
            case (t: TArray, TAnyArray) => TAnyArray
            case (t1: TArray, t2: TArray) => t1 merge t2

            // Unions
            case (t1, t2) => TUnion(t1, t2)
        }
            //println("Joining "+x+" and "+y+", result: "+res)
            res
        }

        // unused
        def meet(x : Type, y : Type) = x
    }

    object BaseTypeEnvironment extends TypeEnvironment(HashMap[CFGSimpleVariable, Type](), None) {
        override def union(e: TypeEnvironment) = {
            e
        }

        override def differsFrom(e: TypeEnvironment): Boolean = {
            e match {
                case BaseTypeEnvironment =>
                    false
                case _ =>
                    true
            }
        }
        override def toString = {
            "[_]"
        }

    }
    class TypeEnvironment(val map: Map[CFGSimpleVariable, Type], val scope: Option[ClassSymbol]) extends Environment[TypeEnvironment] {
        def this(scope: Option[ClassSymbol]) = {
            this(new HashMap[CFGSimpleVariable, Type], scope);
        }
        def this() = {
            this(new HashMap[CFGSimpleVariable, Type], None);
        }

        def lookup(v: CFGSimpleVariable): Option[Type] = map.get(v)

        def inject(v: CFGSimpleVariable, typ: Type): TypeEnvironment = {
            new TypeEnvironment(map + ((v, typ)), scope)
        }

        def union(e: TypeEnvironment) = {
            e match {
                case BaseTypeEnvironment =>
                    this

                case _ =>
                    var newmap = new scala.collection.mutable.HashMap[CFGSimpleVariable, Type]();
                    for ((v,t) <- map) {
                        newmap(v) = TypeLattice.join(t, TNull)
                    }
                    for ((v,t) <- e.map) {
                        if (newmap contains v) {
                            newmap(v) = TypeLattice.join(map(v), t)
                        } else {
                            newmap(v) = TypeLattice.join(t, TNull)
                        }
                    }
                    new TypeEnvironment(Map[CFGSimpleVariable, Type]()++newmap, scope)
            }
        }

        def differsFrom(e: TypeEnvironment): Boolean = {
            e match {
                case BaseTypeEnvironment =>
                    true
                case e: TypeEnvironment =>
                    !(scope equals e.scope) || !(map equals e.map)
            }
        }

        override def toString = {
            map.filter(_._1.toString.toList.head != '_').map(x => x._1+" => "+x._2).mkString("[ ", "; ", " ]");
        }
    }

    case class TypeTransferFunction(silent: Boolean) extends TransferFunction[TypeEnvironment, CFGStatement] {
        //def notice(msg: String, pos: Positional) = if (!silent) { new Exception(msg).printStackTrace(); Reporter.notice(msg, pos) }
        //def error(msg: String, pos: Positional) = if (!silent) { new Exception(msg).printStackTrace(); Reporter.error(msg, pos) }
        def notice(msg: String, pos: Positional) = if (!silent) Reporter.notice(msg, pos)
        def error(msg: String, pos: Positional) = if (!silent) Reporter.error(msg, pos)

        def apply(node : CFGStatement, env : TypeEnvironment) : TypeEnvironment = {
            def typeFromSimpleValue(sv: CFGSimpleValue): Type = sv match {
                case CFGNumLit(value) => TInt
                case CFGStringLit(value) => TString
                case CFGTrue() => TTrue
                case CFGFalse() => TFalse
                case CFGNull() => TNull
                case CFGThis() => getObject(node, env.scope)
                case CFGEmptyArray() => new TArray()
                case CFGInstanceof(lhs, cl) => TBoolean
                case CFGArrayNext(ar) => typeFromSimpleValue(ar)
                case CFGArrayCurElement(id: CFGSimpleVariable) =>
                    env.lookup(id) match {
                        case Some(TAnyArray) =>
                            TAny
                        case Some(t: TArray) =>
                            t.pollutedType match {
                                case Some(pt) =>
                                    pt
                                case None =>
                                    if (t.entries.size > 0) {
                                        t.entries.values.reduceLeft(TypeLattice.join)
                                    } else {
                                        TNull
                                    }
                            }
                        case _ =>
                            TAny
                    }
                case CFGArrayCurElement(ar) => TAny
                case CFGArrayCurKey(ar) => TUnion(TString, TInt)
                case CFGArrayCurIsValid(ar) => expect(ar, TAnyArray); TBoolean
                case CFGNew(cr, params) => cr match {
                    case parser.Trees.StaticClassRef(_, _, id) =>
                        GlobalSymbols.lookupClass(id.value) match {
                            case a @ Some(cs) =>
                                getObject(node, a)
                            case _ =>
                                Predef.error("Undefined class '"+id.value+"'")
                                getObject(node, None)
                        }
                    case _ =>
                        getObject(node, None)
                }
                case fcall @ CFGFunctionCall(id, args) =>
                    GlobalSymbols.lookupFunction(id.value) match {
                        case Some(fs) =>
                                checkFCalls(fcall.params, List(functionSymbolToFunctionType(fs)), fcall)
                        case None =>
                            // handle special functions
                            id.value.toLowerCase match {
                                case "isset" | "empty" =>
                                    TBoolean // no need to check the args, this is a no-error function
                                case _ =>
                                    notice("Function "+id.value+" appears to be undefined!", id)
                                    TNone
                            }
                    }
                case mcall @ CFGMethodCall(r, mid, args) =>
                    expect(r, TAnyObject) match {
                        case or: TObjectRef =>
                            or.lookupMethod(mid.value, env.scope) match {
                                case Some(mt) =>
                                    checkFCalls(args, List(mt), mcall)
                                case None =>
                                    // Check for magic __call ?
                                    val cms = or.lookupMethod("__call", env.scope)
                                    if (cms == None) {
                                        notice("Undefined method '" + mid.value + "' in object "+or, mid)
                                        TNone
                                    } else {
                                        cms.get.ret
                                    }
                            }
                        case _ =>
                            TNone
                    }

                case id: CFGSimpleVariable =>
                  env.lookup(id) match {
                    case Some(t) =>
                        t
                    case None =>
                        notice("Potentially undefined variable "+stringRepr(id), id)
                        TNone
                }

                case CFGArrayEntry(ar, ind) =>
                    expect(ind, TString, TInt)

                    expect(ar, TAnyArray) match {
                        case t: ArrayType =>
                            t.lookup(ind) match {
                                case Some(t) => t
                                case None =>
                                    notice("Potentially undefined array index '"+stringRepr(ind)+"'", ar)
                                    TNone
                            }
                        case TNone => TNone
                        case t =>
                            println("Woops?? invlid type returned from expect: "+t);
                            TNone
                    }

                case op @ CFGObjectProperty(obj, p) =>
                    expect(p, TString);
                    expect(obj, TAnyObject) match {
                        case t: ObjectType =>
                            t.lookupField(p) match {
                                case Some(t) => t
                                case None =>
                                    notice("Potentially undefined object property '"+stringRepr(p)+"'", op)
                                    TNone
                            }
                        case TNone => TNone
                        case t =>
                            println("Woops?? invlid type returned from expect: "+t);
                            TAny
                    }

                case u =>
                  println("Unknown simple value: "+u)
                  TAny
            }

            def getObject(node: CFGStatement, ocs: Option[ClassSymbol]): ObjectType = {
                ObjectStore.getOrCreate(node.uniqueID, ocs)
            }

            def expect(v1: CFGSimpleValue, typs: Type*): Type = {
                val vtyp = typeFromSimpleValue(v1);
                val etyp = typs reduceLeft TypeLattice.join
                /*
                for (t <- typs) {
                    if (TypeLattice.leq(vtyp, t)) {
                        return vtyp
                    }
                }
                */
                if (TypeLattice.leq(vtyp, etyp)) {
                    vtyp
                } else {
                    notice("Potential type mismatch: expected: "+typs.toList.map{x => x.toText}.mkString(" or ")+", found: "+vtyp.toText, v1)
                    typs.toList.head
                }
            }

            def typeCheckBinOP(v1: CFGSimpleValue, op: CFGBinaryOperator, v2: CFGSimpleValue): Type = {
                op match {
                    case PLUS =>
                        expect(v1, TInt); expect(v2, TInt)
                    case MINUS =>
                        expect(v1, TInt); expect(v2, TInt)
                    case MULT =>
                        expect(v1, TInt); expect(v2, TInt)
                    case DIV =>
                        expect(v1, TInt); expect(v2, TInt)
                    case CONCAT =>
                        expect(v1, TString, TInt, TAnyObject); expect(v2, TString, TInt, TAnyObject)
                    case MOD =>
                        expect(v1, TInt); expect(v2, TInt)
                    case INSTANCEOF =>
                        expect(v1, TAnyObject); expect(v2, TString); TBoolean
                    case BOOLEANAND =>
                        expect(v1, TBoolean); expect(v2, TBoolean)
                    case BOOLEANOR =>
                        expect(v1, TBoolean); expect(v2, TBoolean)
                    case BOOLEANXOR =>
                        expect(v1, TBoolean); expect(v2, TBoolean)
                    case BITWISEAND =>
                        expect(v1, TInt); expect(v2, TInt)
                    case BITWISEOR =>
                        expect(v1, TInt); expect(v2, TInt)
                    case BITWISEXOR =>
                        expect(v1, TInt); expect(v2, TInt)
                    case SHIFTLEFT =>
                        expect(v1, TInt); expect(v2, TInt)
                    case SHIFTRIGHT =>
                        expect(v1, TInt); expect(v2, TInt)
                    case LT =>
                        expect(v1, TInt); expect(v2, TInt)
                    case LEQ =>
                        expect(v1, TInt); expect(v2, TInt)
                    case GEQ =>
                        expect(v1, TInt); expect(v2, TInt)
                    case GT =>
                        expect(v1, TInt); expect(v2, TInt)
                    case EQUALS =>
                        expect(v2, expect(v1, TAny)); TBoolean
                    case IDENTICAL =>
                        expect(v2, expect(v1, TAny)); TBoolean
                    case NOTEQUALS =>
                        expect(v2, expect(v1, TAny)); TBoolean
                    case NOTIDENTICAL =>
                        expect(v2, expect(v1, TAny)); TBoolean
                }
          }

          def complexAssign(v: CFGVariable, ext: Type): TypeEnvironment = {
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
                            val rt = ObjectStore.getOrCreate(sv.uniqueID, None).injectField(index, resultType);
                            val ct = if (pass > 0) ObjectStore.getOrCreate(-sv.uniqueID, None).injectField(index, checkType) else TAnyObject;

                            linearize(obj, ct, rt, pass+1)
                        case CFGNextArrayEntry(arr) =>
                            // ditto ArrayEntry
                            val rt = new TArray().injectNext(resultType, arr);
                            val ct = if (pass > 0) new TArray().injectNext(checkType, arr) else TAnyArray;

                            linearize(arr, ct, rt, pass+1)
                        case _ =>
                    }
                }

                // We lineraize the recursive structure
                linearize(v, ext, ext, 0)
                var e = env

                // Let's traverse all up to the last elem (the outermost assign)
                for ((elem, ct, rt) <- elems.init) {
                    //println(" Checking for "+elem +"(actualType: "+typeFromSimpleValue(elem)+", checkType: "+ct+", resultType: "+rt+")");
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

                            // Shouldn't be needed as the resulting
                            // type should never be polluted, by
                            // construction
                            var newPollutedType = (from.pollutedType, to.pollutedType) match {
                                case (Some(pt1), Some(pt2)) => Some(TypeLattice.join(pt1, pt2))
                                case (Some(pt1), None) => Some(pt1)
                                case (None, Some(pt2)) => Some(pt2)
                                case (None, None) => None
                            }

                            val newEntries = HashMap[String, Type]() ++ from.entries;

                            for((index, typ)<- to.entries) {
                                newEntries(index) = newEntries.get(index) match {
                                    case Some(t) => assignMerge(t, typ)
                                    case None => typ
                                }
                            }

                            newPollutedType = newPollutedType match {
                                case Some(pt) =>
                                    for ((index, typ) <- newEntries) {
                                        newEntries(index) = TypeLattice.join(pt, typ)
                                    }
                                    if (newEntries.size > 0) {
                                        Some(newEntries.values reduceLeft TypeLattice.join)
                                    } else {
                                        Some(pt)
                                    }
                                case None => None
                            }

                            new TArray(newEntries, newPollutedType, max(from.nextFreeIndex, to.nextFreeIndex))

                        case (from: TObjectRef, to: TObjectRef) =>
                            import scala.collection.mutable.HashMap
                            //println("Trying to assign-merge "+from+" and "+ to)

                            val newFields = HashMap[String, Type]() ++ from.realObj.fields;

                            for((index, typ)<- to.realObj.fields) {
                                newFields(index) = newFields.get(index) match {
                                    case Some(t) => assignMerge(t, typ)
                                    case None => typ
                                }
                            }

                            val o : RealObjectType = to.realObj match {
                                case o: TRealClassObject =>
                                    TRealClassObject(o.cl, newFields, o.pollutedType)
                                case o: TRealObject =>
                                    TRealObject(newFields, o.pollutedType)
                            }

                            ObjectStore.set(to.id, o)

                            to
                        // In case not both types are not arrays nor objects, we
                        // always end up with the resulting type
                        case (a, b) => a
                    }

                    val resultingType = assignMerge(rt, typeFromSimpleValue(elem))

                    elem match {
                        case sv: CFGSimpleVariable =>
                            // Due to our deep type system, checking
                            // the base variable should be enough
                            expect(elem, ct)
                            e = e.inject(sv, resultingType);
                        case _ =>
                    }
                }

                e
          }

          def functionSymbolToFunctionType(fs: FunctionSymbol): FunctionType = {
            new TFunction(fs.argList.map { a => (a._2.typ, a._2.optional) }, fs.typ)
          }

          def checkFCalls(fcall_params: List[CFGSimpleValue], syms: List[FunctionType], pos: Positional) : Type =  {
            def protoFilter(sym: FunctionType): Boolean = {
                sym match {
                    case TFunction(args, ret) =>
                        var ret = true;
                        for (i <- fcall_params.indices) {
                            if (i >= args.length) {
                                ret = false
                            } else {
                                if (!TypeLattice.leq(typeFromSimpleValue(fcall_params(i)), args(i)._1)) {
                                    //notice("Prototype mismatch because "+fcall.params(i)+"("+typeFromSimpleValue(fcall.params(i))+") </: "+args(i)._1) 

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
                        error("Unmatched function prototype '("+fcall_params.map(typeFromSimpleValue).mkString(", ")+")', candidates are:\n    "+syms.mkString(",\n    "), pos)
                        TNone
                    } else {
                        syms.first match {
                            case TFunction(args, ret) =>
                                for (i <- fcall_params.indices) {
                                    if (i >= args.length) {
                                        error("Prototype error!", pos)
                                    } else {
                                        expect(fcall_params(i), args(i)._1)
                                    }

                                }
                                ret
                            case s =>
                                s.ret
                        }
                    }

                case f :: xs =>
                    f.ret
            }
        }


          node match {
              case CFGAssign(vr: CFGSimpleVariable, v1) =>
                env.inject(vr, typeFromSimpleValue(v1))

              case CFGAssignBinary(vr: CFGSimpleVariable, v1, op, v2) =>
                // We want to typecheck v1/v2 according to OP
                env.inject(vr, typeCheckBinOP(v1, op, v2));

              case CFGAssignBinary(ca: CFGVariable, v1, op, v2) =>
                // We want to typecheck v1/v2 according to OP
                complexAssign(ca, typeCheckBinOP(v1, op, v2))

              case CFGAssign(ca: CFGVariable, ex) =>
                complexAssign(ca, typeFromSimpleValue(ex))

              case CFGAssume(v1, op, v2) => op match {
                  case LT | LEQ | GEQ | GT =>
                    expect(v1, TInt, TFloat); expect(v2, TInt, TFloat); env
                  case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                    /**
                     * Type filtering:
                     * if v1 is a variable that is compared against True/False
                     * we can filter incompatible values out
                     */
                    def filter(v: CFGSimpleVariable, value: Boolean): Type = {
                        typeFromSimpleValue(v) match {
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
                                        TNone
                                    }
                                } else {
                                    if (t != TTrue && t != TResource) {
                                        t
                                    } else {
                                        // we had a single incompatible type
                                        // The branch will never be taken!
                                        notice("Redundant or incompatible check", v);
                                        TNone
                                    }
                                }
                        }
                    }
                    var t1 = v1;
                    var t2 = v2;
                    (v1, typeFromSimpleValue(v1), v2, typeFromSimpleValue(v2)) match {
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
                    (t1, op, typeFromSimpleValue(t2)) match  {
                        case (v: CFGSimpleVariable, EQUALS, TFalse)  =>
                            env.inject(v, filter(v, false))
                        case (v: CFGSimpleVariable, NOTEQUALS, TFalse)  =>
                            env.inject(v, filter(v, true))
                        case (v: CFGSimpleVariable, EQUALS, TTrue)  =>
                            env.inject(v, filter(v, true))
                        case (v: CFGSimpleVariable, NOTEQUALS, TTrue)  =>
                            env.inject(v, filter(v, false))
                        case _ =>
                            expect(v2, expect(v1, TAny)); env
                    }

              }

              case CFGPrint(v) =>
                expect(v, TInt, TString, TAnyObject);
                env
              case CFGUnset(id) =>
                id match {
                    case v: CFGSimpleVariable =>
                        env.inject(v, TNull)
                    case _ =>
                        env // TODO

                }

              case CFGSkip =>
                env

              case ex: CFGSimpleValue =>
                expect(ex, TAny);
                env
              case _ => notice(node+" not yet handled", node); env
          }
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

            scope match {
                case ms: MethodSymbol =>
                    injectPredef("this", ObjectStore.getOrCreate(-1, Some(ms.cs)))
                case _ =>
            }

            injectPredef("_GET",     new TArray(TNone))
            injectPredef("_POST",    new TArray(TNone))
            injectPredef("_REQUEST", new TArray(TNone))
            injectPredef("_COOKIE",  new TArray(TNone))
            injectPredef("_SERVER",  new TArray(TNone))
            injectPredef("_ENV",     new TArray(TNone))
            injectPredef("_SESSION", new TArray(TNone))

            baseEnv
        }

        def analyze = {
            val bottomEnv = BaseTypeEnvironment;
            val baseEnv   = setupEnvironment;

            val aa = new AnalysisAlgorithm[TypeEnvironment, CFGStatement](TypeTransferFunction(true), bottomEnv, baseEnv, cfg)

            aa.init
            aa.computeFixpoint

            if (Main.displayDebug) {
                for ((v,e) <- aa.getResult.toList.sort{(x,y) => x._1.name < y._1.name}) {
                    println("node "+v+" has env "+e);
                }
            }

            aa.pass(TypeTransferFunction(false))
        }
    }
}
