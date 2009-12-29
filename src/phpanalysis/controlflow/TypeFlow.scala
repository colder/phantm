package phpanalysis.controlflow

import CFGTrees._
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import scala.collection.immutable.Map
import analyzer.Symbols._
import analyzer.InternalFunctions
import analyzer.Types._

object TypeFlow {
    case object TypeLattice extends Lattice {
        type E = Type

        def leq(x : Type, y : Type) = (x,y) match {
            case (x, y) if x == y => true

            case (TNone, _) => true
            case (_, TAny) => true
            case (_, TBoolean) => true
            case (t1: TObjectRef, TAnyObject) => true
            case (t1: TPreciseArray, TAnyArray) => true
            case (t1: TPreciseArray, t2: TPreciseArray) =>
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
            case (TAnyArray, t: TPreciseArray) => TAnyArray
            case (t: TPreciseArray, TAnyArray) => TAnyArray
            case (t1: TPreciseArray, t2: TPreciseArray) => t1 merge t2

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
            map.map(x => x._1+" => "+x._2).mkString("[ ", "; ", " ]");
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
                case CFGTrue() => TBoolean
                case CFGFalse() => TBoolean
                case CFGNull() => TNull
                case CFGThis() => getObject(node, env.scope)
                case CFGEmptyArray() => new TPreciseArray()
                case CFGInstanceof(lhs, cl) => TBoolean
                case CFGArrayNext(ar) => typeFromSimpleValue(ar)
                case CFGArrayCurElement(id: CFGSimpleVariable) =>
                    env.lookup(id) match {
                        case Some(TAnyArray) =>
                            TAny
                        case Some(t: TPreciseArray) =>
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
                                checkFCalls(fcall, List(functionSymbolToFunctionType(fs)))
                        case None =>
                            // handle special functions
                            id.value.toLowerCase match {
                                case "isset" | "empty" =>
                                    TBoolean // no need to check the args, this is a no-error function
                                case _ =>
                                    InternalFunctions.lookup(id) match {
                                        case Some(fts) =>
                                            checkFCalls(fcall, fts)
                                        case None =>
                                            notice("Function "+id.value+" appears to be undefined!", id)
                                            TAny
                                    }
                            }
                    }
                case CFGMethodCall(r, mid, p) =>
                    expect(r, TAnyObject) match {
                        case or: TObjectRef =>
                            or.lookupMethod(mid.value, env.scope) match {
                                case Some(mt) =>
                                    // TODO Check for opt args/type hints
                                    mt.ret
                                case None =>
                                    // Check for magic __call ?
                                    if (or.lookupMethod("__call", env.scope) == None) {
                                        notice("Undefined method '" + mid.value + "' in object "+or, mid)
                                        TNull
                                    } else {
                                        TAny
                                    }
                            }
                        case _ =>
                            TNull
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
                        case t: TArray =>
                            t.lookup(ind) match {
                                case Some(t) => t
                                case None =>
                                    notice("Potentially undefined array index "+stringRepr(ind), ar)
                                    TNone
                            }
                        case _ =>
                            println("Woops?? invlid type returned from expect");
                            TAny
                    }

                case CFGObjectProperty(obj, p) =>
                    expect(p, TString);
                    expect(obj, TAnyObject) match {
                        case t: TObject =>
                            TAny
                        case _ =>
                            println("Woops?? invlid type returned from expect");
                            TAny
                    }

                case u =>
                  println("Unknown simple value: "+u)
                  TAny
            }

            def getObject(node: CFGStatement, ocs: Option[ClassSymbol]): TObjectRef = {
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

          def complexAssign(v: CFGVariable, ex: CFGSimpleValue): TypeEnvironment = {
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
                            val rt = new TPreciseArray().inject(index, resultType);
                            val ct = if (pass > 0) new TPreciseArray().inject(index, checkType) else TAnyArray;

                            linearize(arr, ct, rt, pass+1)
                        case CFGNextArrayEntry(arr) =>
                            // ditto ArrayEntry
                            val rt = new TPreciseArray().injectNext(resultType, arr);
                            val ct = if (pass > 0) new TPreciseArray().injectNext(checkType, arr) else TAnyArray;

                            linearize(arr, ct, rt, pass+1)
                        case _ =>
                    }
                }

                // We lineraize the recursive structure
                val ext = typeFromSimpleValue(ex);
                linearize(v, ext, ext, 0)

                var e = env

                // Let's traverse all up to the last elem (the outermost assign)
                for ((elem, ct, rt) <- elems.init) {
                    //println(" Checking for "+elem +"(actualType: "+typeFromSimpleValue(elem)+", checkType: "+ct+", resultType: "+rt+")");
                    val resultingType = (rt, typeFromSimpleValue(elem)) match {
                        // If both the type resulting from the assign and the
                        // previous type are arrays: we merge
                        case (a: TArray, b: TArray) =>
                            // assignMerge will recursively merge types of recursive arrays
                            // we cannot use Lattice.Join as we do not want unions.
                            // i.e. $a['foo']['bar'] = 2; $a['foo']['bar'] =
                            // "str"; should not end with:
                            //    $a -> Array[foo => Array[bar => {TInt, TString}]]
                            // but
                            //    $a -> Array[foo => Array[bar => String]]
                            def assignMerge(from: Type, to: Type): Type = (from,to) match {
                                case (from: TPreciseArray, to: TPreciseArray) =>
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

                                    new TPreciseArray(newEntries, newPollutedType, max(from.nextFreeIndex, to.nextFreeIndex))
                                // In case not both types are not arrays, we
                                // always end up with the target type
                                case (a, b) => b
                            }
                            assignMerge(b, a)
                        case (a: TArray, b) =>
                            rt
                        case _ =>
                            println("Woooops?? Why is that type here, resulting type should be an Array !?")
                            rt
                    }

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
            new TFunction(fs.argList.map { a => (a._4, a._5) }, TAny)
          }

          def checkFCalls(fcall: CFGFunctionCall, syms: List[FunctionType]) : Type =  {
            def protoFilter(sym: FunctionType): Boolean = {
                sym match {
                    case TFunction(args, ret) =>
                        var ret = true;
                        for (i <- fcall.params.indices) {
                            if (i >= args.length) {
                                ret = false
                            } else {
                                if (!TypeLattice.leq(typeFromSimpleValue(fcall.params(i)), args(i)._1)) {
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
                        error("Unmatched function prototype '("+fcall.params.map(typeFromSimpleValue).mkString(", ")+")', candidates are: "+syms.mkString(", "), fcall)
                        TNone
                    } else {
                        syms.first match {
                            case TFunction(args, ret) =>
                                for (i <- fcall.params.indices) {
                                    if (i >= args.length) {
                                        error("Prototype error!", fcall)
                                    } else {
                                        expect(fcall.params(i), args(i)._1)
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
                val e = env.inject(vr, typeFromSimpleValue(v1));
                e
              case CFGAssignBinary(vr: CFGSimpleVariable, v1, op, v2) =>
                // We want to typecheck v1/v2 according to OP
                env.inject(vr, typeCheckBinOP(v1, op, v2));
              case CFGAssignBinary(_, v1, op, v2) =>
                // We want to typecheck v1/v2 according to OP
                typeCheckBinOP(v1, op, v2); env // todo: pollute env

              case CFGAssign(ca: CFGVariable, ex) =>
                complexAssign(ca, ex)

              case CFGAssume(v1, op, v2) => op match {
                  case LT | LEQ | GEQ | GT =>
                    expect(v1, TInt); expect(v2, TInt); env
                  case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                    expect(v2, expect(v1, TAny)); env

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

    case class Analyzer(cfg: CFG, scope: Option[ClassSymbol]) {


        def analyze = {
            val bottomEnv = BaseTypeEnvironment;
            val baseEnv   = new TypeEnvironment;
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
