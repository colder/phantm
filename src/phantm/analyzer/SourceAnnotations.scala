package phantm.analyzer
import phantm.parser.Trees._
import Types._
import Symbols._
import phantm.Reporter
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

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

object AnnotationsParser extends StandardTokenParsers {
    lexical.delimiters += ("[", "]", ",", "=>", "$", "?", "|", "#", "=")
    lexical.reserved   += ("string", "mixed", "long", "int", "false", "true", "null",
                           "number", "integer", "float", "double", "array", "object",
                           "resource", "bool", "boolean", "void", "numeric")

    var typedefs = Map[String, Type]();

    def array: Parser[TArray] =
        "array" ~ "[" ~> repsep(arrayentry, ",") <~ "]" ^^ { 
            case arrentries =>
                var global: Type = TTop
                var entries = Map[String, Type]()

                for ((os, t) <- arrentries) {
                    if (os.isEmpty) {
                        global = t
                    } else {
                        entries += (os.get -> t)
                    }
                }

                new TArray(entries, global)
        } |
        "array" ^^^ TAnyArray

    def arrayentry: Parser[(Option[String], Type)] =
        stringLit ~ "=>" ~ typ ^^ { case key ~ "=>" ~ typ => (Some(key.toString), typ) } |
        "?" ~ "=>" ~> typ ^^ ((None, _: Type))

    def typ: Parser[Type] =
        "string" ^^^ TString |
        "mixed" ^^^ TAny |
        "long" ^^^ TInt |
        "int" ^^^ TInt |
        "false" ^^^ TFalse |
        "true" ^^^ TTrue |
        "null" ^^^ TNull |
        "number" ^^^ TNumeric |
        "integer" ^^^ TInt |
        "float" ^^^ TFloat |
        "double" ^^^ TFloat |
        array |
        "object" ^^^ TAnyObject |
        "resource" ^^^ TResource |
        "bool" ^^^ TBoolean |
        "void" ^^^ TNull |
        "numeric" ^^^ TNumeric |
        "#" ~> ident ^^ { i => typedefs.getOrElse(i.toString, {
            Reporter.notice("Undefined typedef '"+ i.toString+"'")
            TBottom
        })}

    def utyp: Parser[Type] =
        typ ~ rep("|" ~> typ) ^^ { case t ~ ts => TUnion(t :: ts) }

    def variable: Parser[String] =
        "$" ~> ident ^^ (_.toString)

    def typedef: Parser[(String, Type)] =
        ident ~ "=" ~ utyp ^^ { case i ~ "=" ~ t => (i.toString, t) }

    def typVar: Parser[(String, Type)] =
        utyp ~ variable ^^ { case t ~ v => (v, t) }


    // Parsing helpers

    def strToType(str: String): Option[Type] = {
        val s = new lexical.Scanner(str);
        val r = utyp(s)
        if (r.isEmpty) None else Some(r.get)
    }

    def strToVarType(str: String): Option[(String, Type)] = {
        val s = new lexical.Scanner(str);
        val r = typVar(s)
        if (r.isEmpty) None else Some(r.get)
    }

    def importTypeDef(str: String): Unit = {
        val s = new lexical.Scanner(str)
        val r = typedef(s)
        if (!r.isEmpty) {
            val (name, t) = r.get
            typedefs += (name -> t)
        }
    }

}

object Annotations {
    def filterLines(lines: List[String], tag: String): List[String] = {
        lines.filter(_.indexOf(tag+" ") >= 0).map(s => s.substring(s.indexOf(tag+" ")+tag.length+1))
    }

    def paramsTH(lines: List[String]): Map[String, TypeHint] = {
        var res = Map[String, TypeHint]()
        for (str <- filterLines(lines, "@param")) {
            AnnotationsParser.strToVarType(str) match {
                case Some((v, t)) => res += (v -> THType(t))
                case None => /* ignore */
            }
        }
        res
    }

    def parseTypeDefs(comment: String) = {
        val lines = comment.split("\n").toList

        for (l <- filterLines(lines, "@typedef")) {
            AnnotationsParser.importTypeDef(l)
        }

    }

    def returnTH(lines: List[String]): Option[TypeHint] = {
        for (str <- filterLines(lines, "@return")) {
            return AnnotationsParser.strToType(str).map(THType(_))
        }
        return None
    }

    def varTH(lines: List[String]): Option[TypeHint] = {
        for (str <- filterLines(lines, "@return")) {
            return AnnotationsParser.strToType(str).map(THType(_))
        }
        return None
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
    import phantm.controlflow.TypeFlow
    import phantm.analyzer.Types._
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
