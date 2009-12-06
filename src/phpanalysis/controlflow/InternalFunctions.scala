package phpanalysis.controlflow

import Types._
import collection.mutable.HashMap
import parser.Trees._
import analyzer.Symbols._
import scala.xml._

object InternalFunctions {
    private var loaded = false
    private var ftypes = HashMap[String, FunctionType]()

    def strToType(str: String): Type = str.toLowerCase match {
        case "string" => TString
        case "mixed" => TAny
        case "long" => TInt
        case "int" => TInt
        case "number" => TInt
        case "integer" => TInt
        case "float" => TInt
        case "double" => TInt
        case "array" => TAnyArray
        case "object" => TAnyObject
        case "resource" => TResource
        case "bool" => TBoolean
        case "boolean" => TBoolean
        case "void" => TNull
        case _ => /*println("Uh?" + str);*/ TAny
    }

    def load = {
        val data = XML.loadFile("spec/internal_functions.xml")

        for (f <- data \\ "function") {
            val name = (f \ "@name").text
            val returnType = (f \ "return" \ "type").text
            val args: List[(Type, Boolean)] = ((f \ "args" \\ "arg") map { a => (strToType(a.text), Integer.parseInt((a \ "@opt").text) > 0) }).toList

            ftypes(name) = new TFunction(args, strToType(returnType))
        }
        loaded = true;
    }

    def lookup(id: Identifier): Option[FunctionType] = {
        if (!loaded) load
        ftypes.get(id.value)
    }
}
