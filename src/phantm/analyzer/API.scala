package phpanalysis.analyzer

import collection.mutable.HashMap
import phpanalysis.{Positional,Reporter}
import phpanalysis.parser.Trees._
import Symbols._
import Types._
import scala.xml._

// Load an API into the symbol tables
class API(file: String) {
    case class APIPos(elem: Node) extends Positional {
        val pos = (elem \ "position")
        file = (pos \ "@file").text match {
            case "" => None
            case s => Some(s)
        }
        line = (pos \ "@line").text match {
            case "" => -1
            case s => Integer.parseInt(s)
        }
        col = (pos \ "@col").text match {
            case "" => -1
            case s => Integer.parseInt(s)
        }
    }

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
        case typ =>
            TAny
    }

    def load = {
        try {
            val data = XML.loadFile(file)
            val userland = (data \ "@userland") == "yes"

            // classes
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

                val cs = new ClassSymbol(name, pcs, Nil).setPos(APIPos(c))
                cs.setOverwriteable(userland).setUserland(userland)

                // Register class methods
                for (m <- c \\ "method") {
                    val name = (m \ "@name").text

                    val visibility = (m \ "@visibility").text match {
                        case "protected" => MVProtected
                        case "private" => MVPrivate
                        case _ => MVPublic
                    }


                    val args: List[(Type, Boolean)] = ((m \ "args" \\ "arg") map { a => (elemsToType(a \ "type"), Integer.parseInt((a \ "@opt").text) > 0) }).toList

                    val ms = new MethodSymbol(cs, name, visibility, elemsToType(m \ "return" \ "type")).setPos(APIPos(m))
                    ms.setOverwriteable(userland).setUserland(userland)

                    for ((a, i) <- args.zipWithIndex) {
                        val as = new ArgumentSymbol("arg"+i, false, a._2, a._1)
                        as.setOverwriteable(userland).setUserland(userland)
                        ms.registerArgument(as)
                    }
                    cs.registerMethod(ms)
                }

                // Register static fields
                for (f <- c \ "staticfields" \\ "field") {
                    val name = (f \ "@name").text;
                    val visibility = (f \ "@visibility").text match {
                        case "protected" => MVProtected
                        case "private" => MVPrivate
                        case _ => MVPublic
                    }
                    val ps = new PropertySymbol(cs, name, visibility, elemsToType(f \ "type")).setPos(APIPos(f))
                    ps.setOverwriteable(userland).setUserland(userland)

                    cs.registerStaticProperty(ps)
                }

                // Register fields
                for (f <- c \ "fields" \\ "field") {
                    val name = (f \ "@name").text;
                    val visibility = (f \ "@visibility").text match {
                        case "protected" => MVProtected
                        case "private" => MVPrivate
                        case _ => MVPublic
                    }
                    val ps = new PropertySymbol(cs, name, visibility, elemsToType(f \ "type")).setPos(APIPos(f))
                    ps.setOverwriteable(userland).setUserland(userland)

                    cs.registerProperty(ps)
                }

                // Register constants
                for (cc <- c \ "constants" \\ "constant") {
                    val name = (cc \ "@name").text;
                    val ccs = new ClassConstantSymbol(cs, name, None, elemsToType(cc \ "type")).setPos(APIPos(cc))
                    ccs.setOverwriteable(userland).setUserland(userland)

                    cs.registerConstant(ccs)
                }

                GlobalSymbols.registerClass(cs)
            }

            // functions
            for (f <- data \\ "function") {
                val name = (f \ "@name").text
                val args: List[(Node, Type, Boolean)] = ((f \ "args" \\ "arg") map { a => (a, elemsToType(a \ "type"), Integer.parseInt((a \ "@opt").text) > 0) }).toList

                val fs = new FunctionSymbol(name, elemsToType(f \ "return" \ "type")).setPos(APIPos(f))
                fs.setOverwriteable(userland).setUserland(userland)

                for ((a, i) <- args.zipWithIndex) {
                    val as = new ArgumentSymbol("arg"+i, false, a._3, a._2).setPos(APIPos(a._1))
                    as.setOverwriteable(userland).setUserland(userland)

                    fs.registerArgument(as)
                }
                GlobalSymbols.lookupFunction(name) match {
                    case Some(fs) =>
                        // TODO: Add prototype
                    case None =>
                        GlobalSymbols.registerFunction(fs)
                }
            }

            for (cc <- data \ "constants" \\ "constant") {
                val name = (cc \ "@name").text;
                val ccs = new ConstantSymbol(name, None, elemsToType(cc \ "type")).setPos(APIPos(cc))
                ccs.setOverwriteable(userland).setUserland(userland)

                GlobalSymbols.registerConstant(ccs)
            }

        } catch {
            case e =>
                Reporter.error("Parsing of the api file '"+file+"' failed: "+e.getMessage)
        }
    }


    def write = {

    }
}
