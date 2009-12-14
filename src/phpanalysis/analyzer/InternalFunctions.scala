package phpanalysis.analyzer

import collection.mutable.HashMap
import parser.Trees._
import Symbols._
import Types._
import scala.xml._

object InternalFunctions {
    private var loaded = false
    private var ftypes = HashMap[String, List[FunctionType]]()

    def elemsToType(elems: NodeSeq): Type = elems.map { e => elemToType(e) } reduceRight { (a, b) => TUnion(a, b) }
    def elemToType(elem: Node): Type = (elem \ "@name").text.toLowerCase match {
        case "string" => TString
        case "mixed" => TAny
        case "long" => TInt
        case "int" => TInt
        case "null" => TNull
        case "number" => TInt
        case "integer" => TInt
        case "float" => TInt
        case "double" => TInt
        case "array" =>
            if ((elem \ "type").length > 0) {
                new TPreciseArray(elemsToType(elem \ "type"))
            } else {
                TAnyArray
            }
        case "object" =>
            TAnyObject
        case "resource" => TResource
        case "bool" => TBoolean
        case "boolean" => TBoolean
        case "void" => TNull
        case _ => TAny
    }

    def load = {
        val data = XML.loadFile("spec/internal_functions.xml")

        for (f <- data \\ "function") {
            val name = (f \ "@name").text
            val args: List[(Type, Boolean)] = ((f \ "args" \\ "arg") map { a => (elemsToType(a \ "type"), Integer.parseInt((a \ "@opt").text) > 0) }).toList

            ftypes(name) = new TFunction(args, elemsToType(f \ "return" \ "type")) :: ftypes.getOrElse(name, Nil)
        }
        loaded = true;
    }

    def lookup(id: Identifier): Option[List[FunctionType]] = {
        if (!loaded) load
        ftypes.get(id.value)
    }
}
