package phpanalysis.controlflow

import CFGTrees._
import scala.collection.immutable.HashMap
import scala.collection.immutable.Map
import Types._

case class TypeEnvironment(map: Map[CFGSimpleVariable, Type]) extends Environment[TypeEnvironment] {
    def this() = {
        this(new HashMap[CFGSimpleVariable, Type]);
    }

    def lookup(v: CFGSimpleVariable): Option[Type] = map.get(v)

    def inject(v: CFGSimpleVariable, typ: Type): TypeEnvironment = {
        TypeEnvironment(map + ((v, typ)))
    }

    def union(e: TypeEnvironment) = {
        val newmap = new scala.collection.mutable.HashMap[CFGSimpleVariable,Type]();
        for ((v,t) <- map) {
            newmap(v) = t
        }
        //println("Union of "+this+" and "+e+"...")
        for ((v,t) <- e.map) {
            if (newmap contains v) {
         //       println("colliding vars!:"+v)
                newmap(v) = newmap(v) union t
            } else {
                newmap(v) = t
            }
        }
        val res = new TypeEnvironment(Map[CFGSimpleVariable, Type]()++newmap)
        //println("Result: "+res);
        res
    }

    def equals(e: TypeEnvironment): Boolean = {
        if (e.map.size != map.size) return false
        for ((v,t) <- map) {
            if (!(e.map contains v)) return false
            else if (!(e.map(v) equals t)) return false
        }

        return true
    }

    override def toString = {
        map.map(x => x._1+" => "+x._2).mkString("[ ", "; ", " ]");
    }
}

case class TypeTransferFunction(silent: Boolean) extends TransferFunction[TypeEnvironment, CFGStatement] {
    def notice(msg: String, pos: Positional) = if (!silent) Reporter.notice(msg, pos)


    def apply(node : CFGStatement, env : TypeEnvironment) : TypeEnvironment = {
        def typeFromSimpleValue(sv: CFGSimpleValue): Type = sv match {
            case CFGNumLit(value) => TInt
            case CFGStringLit(value) => TString
            case CFGTrue() => TBoolean
            case CFGFalse() => TBoolean
            case CFGNull() => TNull
            case CFGThis() => TAnyObject
            case CFGEmptyArray() => TArray
            case CFGInstanceof(lhs, cl) => TBoolean
            case CFGArrayNext(ar) => TAny
            case CFGArrayCurElement(ar) => TAny
            case CFGArrayCurKey(ar) => TAny
            case CFGArrayCurIsValid(ar) => TAny
            case CFGNew(tpe, params) => TAnyObject
            case id @ CFGIdentifier(vs) =>
              env.lookup(id) match {
                  case Some(t) => t
                  case None => TAny
              }
            case tid @ CFGTempID(vs) =>
              env.lookup(tid) match {
                  case Some(t) => t
                  case None => TAny
              }
            case _ =>
              TAny
        }

        def expect(v1: CFGSimpleValue, typ: Type): Type = {
            val vtyp = typeFromSimpleValue(v1);
            if (typ contains vtyp) {
                vtyp
            } else {
                notice("Potential type mismatch: expected: "+typ+", found: "+vtyp.toText, v1)
                TAny
            }
        }

        def typeCheckBinOP(v1: CFGSimpleValue, op: CFGBinaryOperator, v2: CFGSimpleValue): Type = {
            op match {
                case PLUS =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case MINUS =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case MULT =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case DIV =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case CONCAT =>
                    expect(v1, TString); expect(v2, TString); TString
                case MOD =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case INSTANCEOF =>
                    expect(v1, TAnyObject); expect(v2, TString); TBoolean
                case BOOLEANAND =>
                    expect(v1, TBoolean); expect(v2, TBoolean); TBoolean
                case BOOLEANOR =>
                    expect(v1, TBoolean); expect(v2, TBoolean); TBoolean
                case BOOLEANXOR =>
                    expect(v1, TBoolean); expect(v2, TBoolean); TBoolean
                case BITWISEAND =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case BITWISEOR =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case BITWISEXOR =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case SHIFTLEFT =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case SHIFTRIGHT =>
                    expect(v1, TInt); expect(v2, TInt); TInt
                case LT =>
                    expect(v1, TInt); expect(v2, TInt); TBoolean
                case LEQ =>
                    expect(v1, TInt); expect(v2, TInt); TBoolean
                case GEQ =>
                    expect(v1, TInt); expect(v2, TInt); TBoolean
                case GT =>
                    expect(v1, TInt); expect(v2, TInt); TBoolean
                case EQUALS =>
                    expect(v2, expect(v1, TAny)); TBoolean
                case IDENTICAL =>
                    expect(v2, expect(v1, TAny)); TBoolean
                case NOTEQUALS =>
                    expect(v2, expect(v1, TAny)); TBoolean
                case NOTIDENTICAL =>
                    expect(v2, expect(v1, TAny)); TBoolean
                case OBJECTREAD =>
                    expect(v1, TAnyObject); TAny
                case ARRAYREAD =>
                    expect(v1, TAny); TAny
            }
      }

      node match {
          case CFGAssign(vr: CFGSimpleVariable, v1) =>
            env.inject(vr, typeFromSimpleValue(v1));
          case CFGAssignBinary(vr: CFGSimpleVariable, v1, op, v2) =>
            // We want to typecheck v1/v2 according to OP
            env.inject(vr, typeCheckBinOP(v1, op, v2));
          case CFGAssignBinary(_, v1, op, v2) =>
            // We want to typecheck v1/v2 according to OP
            typeCheckBinOP(v1, op, v2); env // todo: pollute env

          case CFGAssignObjectProperty(obj, prop, ex) =>
            //expect(obj, TAnyObject);
            env

          case CFGAssignArrayNext(arr, expr) =>
            expect(arr, TArray); env

          case CFGAssignArray(arr, index, expr) =>
            expect(arr, TArray); env

          case CFGAssignMethodCall(v, r, mid, p) =>
            expect(r, TAnyObject); env

          case CFGAssume(v1, op, v2) => op match {
              case LT | LEQ | GEQ | GT =>
                expect(v1, TInt); expect(v2, TInt); env
              case EQUALS | IDENTICAL | NOTEQUALS | NOTIDENTICAL =>
                expect(v2, expect(v1, TAny)); env

          }

          case _ => println(node+" not yet handled"); env
      }
  }
}

case class TypeFlowAnalyzer(cfg: CFG) {


    def analyze = {
        val baseEnv = new TypeEnvironment;
        val aa = new AnalysisAlgoritm[TypeEnvironment, CFGStatement](TypeTransferFunction(true), baseEnv, cfg)

        aa.init
        aa.computeFixpoint

        /*
        for ((v,e) <- aa.getResult) {
            println("node "+v+" has env "+e);
        } */

        aa.pass(TypeTransferFunction(false))
    }
}
