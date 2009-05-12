package phpanalysis.analyzer

import scala.collection.mutable.HashMap
import Types._


object Symbols extends Reporter {

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


  class Scope {
    var variables: HashMap[String,VariableSymbol] = new HashMap[String,VariableSymbol]

    def lookupVariable(n: String): Option[VariableSymbol] = variables.get(n)

    def registerVariable(cs: VariableSymbol) : Unit = variables.get(cs.name) match {
      case None => variables += ((cs.name, cs))
      case Some(x) => /* no error */
    }
  }

  class Global {
    var classes: HashMap[String,ClassSymbol] = new HashMap[String,ClassSymbol]
    var functions: HashMap[String,FunctionSymbol] = new HashMap[String,FunctionSymbol]
    var constants: HashMap[String,ConstantSymbol] = new HashMap[String,ConstantSymbol]
    val scope = new Scope

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)

    def registerClass(cs: ClassSymbol) : Unit = classes.get(cs.name) match {
      case None => classes += ((cs.name, cs))
      case Some(x) => error("Class " + cs.name + " already declared (previously declared at "+x+")", cs)
    }

    def lookupFunction(n: String): Option[FunctionSymbol] = functions.get(n)

    def registerFunction(fs: FunctionSymbol) : Unit = functions.get(fs.name) match {
      case None => functions += ((fs.name, fs))
      case Some(x) => error("Function " + fs.name + " already declared (previously declared at "+x+")", fs)
    }

    def lookupConstant(n: String): Option[ConstantSymbol] = constants.get(n)

    def registerConstant(cs: ConstantSymbol) : Unit = constants.get(cs.name) match {
      case None => constants += ((cs.name, cs))
      case Some(x) => error("Function " + cs.name + " already declared (previously declared at "+x+")", cs)
    }
  }

  class FunctionSymbol(val name: String) extends Symbol with Typed {
    val scope = new Scope
  }

  class MethodSymbol(val name: String) extends Symbol with Typed {
    val scope = new Scope
  }

  class InterfaceSymbol(val name: String, extend: List[String]) extends Symbol with Typed {
  }
  class ClassSymbol(val name: String, parent: Option[String], ifaces: List[String]) extends Symbol with Typed
  class ConstantSymbol(val name: String) extends Symbol with Typed
  class VariableSymbol(val name: String) extends Symbol with Typed


}
