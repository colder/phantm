package phpanalysis.analyzer
import phpanalysis.parser.Trees._
import Symbols._
import scala.collection.mutable.{Map,HashMap}

class Annotations(ast: Program) extends ASTTransform(ast) {
    override def trMethod(md: MethodDecl): MethodDecl =
        Annotations.importMethodAnnotations(super.trMethod(md), md.comment)

    override def trProperty(pd: PropertyDecl): PropertyDecl =
        Annotations.importPropertyAnnotations(super.trProperty(pd), pd.comment)

    override def trStmts(stmts: List[Statement]): List[Statement] = stmts match {
        case (fd: FunctionDecl) :: stmts =>
            Annotations.importFunctionAnnotations(fd, fd.comment) :: trStmts(stmts)
        case x =>
            super.trStmts(x)
    }
}

object Annotations {
    def extractType(str: String): (String, TypeHint) = {
        val parts = str.split("[^a-zA-Z0-9_\\|\\$]", 2).toList

        def strToTH(str: String): TypeHint = str.toLowerCase match {
            case "string" => THString
            case "mixed" => THAny
            case "long" => THInt
            case "int" => THInt
            case "false" => THFalse
            case "true" => THTrue
            case "null" => THNull
            case "number" => THInt
            case "integer" => THInt
            case "float" => THFloat
            case "double" => THFloat
            case "array" => THArray
            case "object" => THAnyObject
            case "resource" => THResource
            case "bool" => THBoolean
            case "boolean" => THBoolean
            case "void" => THNull
            case "numeric" => THNumeric
            case cl => THObject(StaticClassRef(NSNone, Nil, Identifier(cl)))
        }

        val th = parts.head.split("\\|").toList.map(strToTH).reduceRight( THUnion )


        (parts.tail.mkString, th)
    }

    def extractTypeName(str: String): (String, TypeHint) = {
        extractType(str) match {
            case (str, th) =>
                val v = str.split("[^\\$a-zA-Z0-9_]", 2).toList.head;
                if (v.length > 0 && v.substring(0, 1) == "$") {
                    (v.substring(1), th)
                } else {
                    (v, th)
                }
        }
    }

    def filterLines(lines: List[String], tag: String): List[String] = {
        lines.filter(_.indexOf(tag+" ") >= 0).map(s => s.substring(s.indexOf(tag+" ")+tag.length+1))
    }

    def paramsTH(lines: List[String]): Map[String, TypeHint] = {
        val res = HashMap[String, TypeHint]();
        for ((v, th) <- filterLines(lines, "@param") map extractTypeName) {
            res(v) = th;
        }
        res
    }

    def returnTH(lines: List[String]): Option[TypeHint] = {
        var res: Option[TypeHint] = None;
        for ((v, th) <- filterLines(lines, "@return") map extractType) {
            res = Some(th);
        }
        res
    }

    def varTH(lines: List[String]): Option[TypeHint] = {
        var res: Option[TypeHint] = None;
        for ((v, th) <- filterLines(lines, "@var") map extractType) {
            res = Some(th);
        }
        res
    }

    def injectArgsHints(args: List[ArgumentDecl], hints: Map[String, TypeHint]): List[ArgumentDecl] =
        args.map { a => ArgumentDecl(a.v, hints.get(a.v.name.value), a.default, a.byref).setPos(a) }

    def importFunctionAnnotations(fd: FunctionDecl, comment: Option[String]): FunctionDecl = {
        // Here we import the annotations as type hints for functions
        if (comment != None) {
            val lines = comment.get.split("\n").toList
            FunctionDecl(fd.name, injectArgsHints(fd.args, paramsTH(lines)), fd.retref, returnTH(lines), fd.body).setPos(fd)
        } else {
            FunctionDecl(fd.name, fd.args, fd.retref, None, fd.body).setPos(fd)
        }
    }

    def importMethodAnnotations(md: MethodDecl, comment: Option[String]): MethodDecl = {
        // Here we import the annotations as type hints for methods
        if (comment != None) {
            val lines = comment.get.split("\n").toList
            MethodDecl(md.name, md.flags, injectArgsHints(md.args, paramsTH(lines)), md.retref, returnTH(lines), md.body).setPos(md)
        } else {
            MethodDecl(md.name, md.flags, md.args, md.retref, None, md.body).setPos(md)
        }
    }

    def importPropertyAnnotations(pd: PropertyDecl, comment: Option[String]): PropertyDecl = {
        // Here we import the annotations as type hints for properties
        if (comment != None) {
            val lines = comment.get.split("\n").toList
            PropertyDecl(pd.v, pd.flags, pd.default, varTH(lines)).setPos(pd)
        } else {
            PropertyDecl(pd.v, pd.flags, pd.default, None).setPos(pd)
        }
    }
}

object AnnotationsExport {
    import phpanalysis.controlflow.TypeFlow
    import phpanalysis.analyzer.Types._
    // Compacts collected annotations and exports them

    def reduceFT(ft1: TFunction, ft2: TFunction): TFunction = {
        new TFunction(ft1.args.zipAll(ft2.args, (TBottom, true), (TBottom, true)).map {
            a => (a._1._1 union a._2._1, a._1._2 || a._2._2)
        }, ft1.ret union ft2.ret)
    }


    def emitXML(path: String) = {
        def typeToXML(typ: Type): String  = {
            def simpleTyp(name: String) = "<type name=\""+name+"\" />"

            typ match {
                case TInt => simpleTyp("int")
                case TNumeric => simpleTyp("numeric")
                case TBoolean => simpleTyp("bool")
                case TTrue => simpleTyp("true")
                case TFalse => simpleTyp("false")
                case TFloat => simpleTyp("float")
                case TString => simpleTyp("string")
                case TAny => simpleTyp("any")
                case TResource => simpleTyp("resource")
                case TNull => simpleTyp("null")
                case tor: TObjectRef => simpleTyp("object")
                case TAnyObject => simpleTyp("object")
                case tu: TUnion =>
                    tu.types.map(typeToXML).mkString
                case ta: TArray =>
                    ta.globalType match {
                        case TTop =>
                            simpleTyp("array")
                        case TUninitialized =>
                            simpleTyp("array")
                        case gt =>
                            "<type name=\"array\">"+typeToXML(gt)+"</type>"
                    }
                case _ => println("Unknown Type: "+typ); simpleTyp("any")
            }
        }
        val outputStream = new java.io.FileOutputStream(path);
        val printStream  = new java.io.PrintStream(outputStream);

        def emit(str: String) = printStream.println(str)

        emit("<!-- Generated API -->")
        emit("<api userland=\"yes\">")

        // functions
        emit(" <functions>")
        for ((name, data) <- TypeFlow.AnnotationsStore.functions) {
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
