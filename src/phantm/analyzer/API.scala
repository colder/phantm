package phantm.analyzer

import phantm.util.{Positional,Reporter}
import phantm.AST.Trees._
import phantm.symbols._
import Types._
import scala.xml._

import phantm.controlflow.AnnotationsStore

object API {
    // Load an API into the symbol tables
    class Reader(file: String) {
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
            case "string" => elem.attribute("value") match {
                case Some(s) =>
                    TStringLit(s.head.text)
                case None =>
                    TString
            }
            case "long" | "int" | "integer" => elem.attribute("value") match {
                case Some(s) =>
                    TIntLit(s.head.text.toInt)
                case None =>
                    TInt
            }
            case "float" | "double" => elem.attribute("value") match {
                case Some(s) =>
                    TFloatLit(s.head.text.toFloat)
                case None =>
                    TFloat
            }
            case "mixed" => TAny
            case "false" => TFalse
            case "true" => TTrue
            case "null" => TNull
            case "number" => TNumeric
            case "array" =>
                var anyelem: Type = TTop
                var elems = Map[String, Type]()

                for (el <- (elem \ "elem")) {
                    elems += (((el \ "@key").text) -> elemsToType(el \ "type"))
                }

                for (el <- (elem \ "anyelem")) {
                    anyelem = elemsToType(el \ "type")
                }

                if (elems.size == 0 && anyelem == TTop) {
                    TAnyArray
                } else {
                    new TArray(elems, anyelem)
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

                        val ms = new MethodSymbol(cs, name, visibility).setPos(APIPos(m))
                        ms.setOverwriteable(userland).setUserland(userland)

                        for ((a, i) <- args.zipWithIndex) {
                            val as = new ArgumentSymbol("arg"+i, false, a._2)
                            as.typ = a._1
                            as.setOverwriteable(userland).setUserland(userland)
                            ms.registerArgument(as)
                        }
                        ms.registerFType(TFunction(ms.argList.map { a => (a._2.typ, a._2.optional) }, elemsToType(m \ "type")))
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
                        val ps = new PropertySymbol(cs, name, visibility).setPos(APIPos(f))
                        ps.typ = elemsToType(f \ "type")
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
                        val ps = new PropertySymbol(cs, name, visibility).setPos(APIPos(f))
                        ps.typ = elemsToType(f \ "type")
                        ps.setOverwriteable(userland).setUserland(userland)

                        cs.registerProperty(ps)
                    }

                    // Register constants
                    for (cc <- c \ "constants" \\ "constant") {
                        val name = (cc \ "@name").text;
                        val ccs = new ClassConstantSymbol(cs, name, None).setPos(APIPos(cc))
                        ccs.typ = elemsToType(cc \ "type")
                        ccs.setOverwriteable(userland).setUserland(userland)

                        cs.registerConstant(ccs)
                    }

                    GlobalSymbols.registerClass(cs)
                }

                // functions
                for (f <- data \\ "function") {
                    val name = (f \ "@name").text
                    val args: List[(Node, Type, Boolean)] = ((f \ "args" \\ "arg") map { a => (a, elemsToType(a \ "type"), Integer.parseInt((a \ "@opt").text) > 0) }).toList

                    val fs = new FunctionSymbol(name).setPos(APIPos(f))
                    fs.setOverwriteable(userland).setUserland(userland)

                    for ((a, i) <- args.zipWithIndex) {
                        val as = new ArgumentSymbol("arg"+i, false, a._3).setPos(APIPos(a._1))
                        as.typ = a._2
                        as.setOverwriteable(userland).setUserland(userland)

                        fs.registerArgument(as)
                    }

                    fs.registerFType(TFunction(fs.argList.map { a => (a._2.typ, a._2.optional) }, elemsToType(f \ "type")))

                    GlobalSymbols.lookupFunction(name) match {
                        case Some(fs) =>
                            // TODO: Add prototype
                        case None =>
                            GlobalSymbols.registerFunction(fs)
                    }
                }

                for (cc <- data \ "constants" \\ "constant") {
                    val name = (cc \ "@name").text;
                    val ccs = new ConstantSymbol(name, None).setPos(APIPos(cc))
                    ccs.typ = elemsToType(cc \ "type")
                    ccs.setOverwriteable(userland).setUserland(userland)

                    GlobalSymbols.registerConstant(ccs)
                }

            } catch {
                case e =>
                    Reporter.error("Parsing of the api file '"+file+"' failed: "+e.getMessage)
            }
        }
    }

    class Writer(path: String) {
        // Compacts collected annotations and exports them
        def reduceFT(ft1: TFunction, ft2: TFunction): TFunction = {
            new TFunction(ft1.args.zipAll(ft2.args, (TBottom, true), (TBottom, true)).map {
                a => (a._1._1 union a._2._1, a._1._2 || a._2._2)
            }, ft1.ret union ft2.ret)
        }


        def emitXML = {
            def typeToXML(typ: Type): String  = {
                def simpleTyp(name: String) = "<type name=\""+name+"\" />"
                def simpleTypVal(name: String, value: String) = "<type name=\""+name+"\" value=\""+value+"\" />"

                typ match {
                    case TInt            => simpleTyp("int")
                    case TIntLit(i)      => simpleTypVal("int", i.toString)
                    case TNumeric        => simpleTyp("numeric")
                    case TBoolean        => simpleTyp("bool")
                    case TTrue           => simpleTyp("true")
                    case TFalse          => simpleTyp("false")
                    case TFloat          => simpleTyp("float")
                    case TFloatLit(l)    => simpleTypVal("float", l.toString)
                    case TString         => simpleTyp("string")
                    case TStringLit(s)   => simpleTypVal("string", s)
                    case TAny            => simpleTyp("any")
                    case TResource       => simpleTyp("resource")
                    case TNull           => simpleTyp("null")
                    case tor: TObjectRef => simpleTyp("object")
                    case TAnyObject      => simpleTyp("object")
                    case tu: TUnion      =>
                        tu.types.map(typeToXML).mkString

                    case ta: TArray      =>
                        val es = ta.entries.map(e => "<elem key=\""+e._1+"\">"+typeToXML(e._2)+"</elem>").mkString
                        val ge = "<anyelem>"+typeToXML(ta.globalType)+"</anyelem>"
                        "<type name=\"array\">"+es+ge+"</type>"

                    case _               =>
                        println("Unknown Type: "+typ); simpleTyp("any")
                }
            }
            val outputStream = new java.io.FileOutputStream(path);
            val printStream  = new java.io.PrintStream(outputStream);

            def emit(str: String) = printStream.println(str)

            emit("<!-- Generated API -->")
            emit("<api userland=\"yes\">")

            // functions
            emit(" <functions>")
            for ((name, data) <- AnnotationsStore.functions) {
                GlobalSymbols.lookupFunction(name) match {
                    case Some(fs) if fs.userland =>
                        val args = if (data._1.size > 0) (data._1 reduceLeft reduceFT).args else Nil;
                        val ret  = data._2;

                        emit("  <function name=\""+name+"\">")
                        emit("   <return>"+typeToXML(ret)+"</return>")
                        emit("   <args>")
                        for (arg <- args) {
                            emit("    <arg opt=\""+(if (arg._2) "1" else "0")+"\">"+typeToXML(arg._1)+"</arg>")
                        }
                        emit("   </args>")
                        emit("  </function>")
                    case _ =>
                        // ignore
                }
            }
            emit(" </functions>")
            emit("</api>")
        }
    }
}
