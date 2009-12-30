package phpanalysis.analyzer

import collection.mutable.HashMap
import parser.Trees._
import Symbols._
import Types._
import scala.xml._

// Load an API into the symbol tables
class API(file: String) {

    def elemsToType(elems: NodeSeq): Type = {
        if (elems.size > 0) {
            elems.map { e => elemToType(e) } reduceRight { (a, b) => TUnion(a, b) }
        } else {
            TAny
        }
    }
    def elemToType(elem: Node): Type = (elem \ "@name").text.toLowerCase match {
        case "string" => TString
        case "mixed" => TAny
        case "long" => TInt
        case "int" => TInt
        case "false" => TFalse
        case "true" => TTrue
        case "null" => TNull
        case "number" => TInt
        case "integer" => TInt
        case "float" => TFloat
        case "double" => TFloat
        case "array" =>
            if ((elem \ "type").length > 0) {
                new TArray(elemsToType(elem \ "type"))
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
        try {
        val data = XML.loadFile(file)
        for (c <- data \\ "class") {
            val name = (c \ "@name").text
            val parent = (c \ "@parent").text

            val pcs = if (parent != "") {
                GlobalSymbols.lookupClass(parent) match {
                    case Some(ocs) =>
                        Some(ocs)
                    case None =>
                        Reporter.error("Error loading class '"+name+"' from API: parent class '"+parent+"' not found")
                        None
                }
            } else {
                None
            }

            val cs = new ClassSymbol(name, pcs, Nil)

            for (m <- c \\ "method") {
                val name = (m \ "@name").text

                var visibility = (m \ "@visibility").text match {
                    case "protected" => MVProtected
                    case "private" => MVPrivate
                    case _ => MVPublic
                }

                /*
                 TODO: distinguish static methods
                var type = (m \ "@type").text match {
                    case "static" => "static"
                    case _ => "normal"
                }
                */

                val args: List[(Type, Boolean)] = ((m \ "args" \\ "arg") map { a => (elemsToType(a \ "type"), Integer.parseInt((a \ "@opt").text) > 0) }).toList

                val ms = new MethodSymbol(cs, name, elemsToType(m \ "return" \ "type"), visibility)
                for ((a, i) <- args.zipWithIndex) {
                    val vs = new VariableSymbol("arg"+i)
                    ms.registerArgument(vs, false, a._1, a._2)
                }
                cs.registerMethod(ms)
            }

            GlobalSymbols.registerClass(cs)
        }

        for (f <- data \\ "function") {
            val name = (f \ "@name").text
            val args: List[(Type, Boolean)] = ((f \ "args" \\ "arg") map { a => (elemsToType(a \ "type"), Integer.parseInt((a \ "@opt").text) > 0) }).toList

            val fs = new FunctionSymbol(name, elemsToType(f \ "return" \ "type"))
            for ((a, i) <- args.zipWithIndex) {
                val vs = new VariableSymbol("arg"+i)
                fs.registerArgument(vs, false, a._1, a._2)
            }
            GlobalSymbols.lookupFunction(name) match {
                case Some(fs) =>
                    // TODO: Add prototype
                case None =>
                    GlobalSymbols.registerFunction(fs)
            }
        }

        } catch {
            case e: org.xml.sax.SAXParseException => 
                Reporter.error("Parsing of the api file '"+file+"' failed: "+e.getMessage)
        }
    }
}
