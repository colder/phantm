package phpanalysis.analyzer

import scala.collection.mutable.HashMap
import Types._


object Symbols extends Reporter {
  trait Symbolic {
    self =>

    private var _sym: Option[Symbol] = None

    def setSymbol(sym: Symbol): self.type = { _sym = Some(sym); this }
    def getSymbol: Symbol = _sym match {
        case Some(x) => x
        case None => scala.Predef.error("Cannot access undefined Symbol")
    }
  }

  abstract class Symbol extends Positional {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }


  trait Scope {
    var variables: HashMap[String,VariableSymbol] = new HashMap[String,VariableSymbol]

    def getVariables: List[VariableSymbol] = variables map { x => x._2 } toList

    def lookupVariable(n: String): Option[VariableSymbol] = variables.get(n)

    def registerVariable(cs: VariableSymbol) : Unit = variables.get(cs.name) match {
      case None => variables += ((cs.name, cs))
      case Some(x) => /* no error */
    }
  }

  object GlobalSymbols extends Scope {
    var classes: HashMap[String,ClassSymbol] = new HashMap[String,ClassSymbol]
    var functions: HashMap[String,FunctionSymbol] = new HashMap[String,FunctionSymbol]
    var constants: HashMap[String,ConstantSymbol] = new HashMap[String,ConstantSymbol]

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)

    def registerClass(cs: ClassSymbol) : Unit = classes.get(cs.name) match {
      case None => classes += ((cs.name, cs))
      case Some(x) => error("Class " + cs.name + " already declared (previously declared in "+x.getPos+")", cs)
    }

    def lookupFunction(n: String): Option[FunctionSymbol] = functions.get(n)

    def registerFunction(fs: FunctionSymbol) : Unit = functions.get(fs.name) match {
      case None => functions += ((fs.name, fs))
      case Some(x) => error("Function " + fs.name + " already declared (previously declared in "+x.getPos+")", fs)
    }

    def lookupConstant(n: String): Option[ConstantSymbol] = constants.get(n)

    def registerConstant(cs: ConstantSymbol) : Unit = constants.get(cs.name) match {
      case None => constants += ((cs.name, cs))
      case Some(x) => error("Function " + cs.name + " already declared (previously declared in "+x.getPos+")", cs)
    }
  }

  class FunctionSymbol(val name: String) extends Symbol with Typed with Scope {
    val args = new HashMap[String, (VariableSymbol, Boolean)]();

    def registerArgument(vs: VariableSymbol, byref: Boolean) = args.get(vs.name) match {
        case Some(x) => error("Argument "+vs.name+" already defined (previously defined in "+x._1.getPos+")", vs)
        case None => args += ((vs.name, (vs, byref)))

    }
    override def getVariables: List[VariableSymbol] = (args map { x => x._2._1} toList) ::: super.getVariables

    override def lookupVariable(n: String): Option[VariableSymbol] = args.get(n) match {
        case Some((vs, byref)) => Some(vs)
        case None => variables.get(n)
    }

    override def registerVariable(vs: VariableSymbol) : Unit = args.get(vs.name) match {
        case Some(x) =>
        case None    =>
          variables.get(vs.name) match {
            case None => variables += ((vs.name, vs))
            case Some(x) => /* no error */
          }
    }
  }

  class MethodSymbol(name: String) extends FunctionSymbol(name) {

  }

  class InterfaceSymbol(val name: String, extend: List[String]) extends Symbol with Typed
  class ClassSymbol(val name: String, parent: Option[String], ifaces: List[String]) extends Symbol with Typed
  class ConstantSymbol(val name: String) extends Symbol with Typed
  class VariableSymbol(val name: String) extends Symbol with Typed

  def emitSummary = {
        def emitScope(s: Scope) = {
            for (val v <- s.getVariables) {
                println("Var "+v.name+"("+v.id+")")
            }
        }
        for (val cs <- GlobalSymbols.classes) {
            println("----");
            println("Class: "+cs._1)
            println("----");
        }

        for (val fs <- GlobalSymbols.functions) {
            println("----");
            println("Function: "+fs._1)
            println("Scope:");
            emitScope(fs._2);
            println("----");
        }

        emitScope(GlobalSymbols)
  }
}
