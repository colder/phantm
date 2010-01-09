package phpanalysis.analyzer
import parser.Trees._
import scala.collection.mutable.{Map,HashMap}

class Annotations(ast: Program) extends ASTTransform(ast) {
    override def trStmts(stmts: List[Statement]): List[Statement] = stmts match {
        case DocComment(comment) :: (fd: FunctionDecl) :: sts =>
            trStmt(Annotations.importFunctionsAnnotations(fd, comment)) :: trStmts(sts)
        case ts :: sts =>
            trStmt(ts) :: trStmts(sts)
        case Nil =>
            Nil

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
            case cl => THObject(StaticClassRef(NSNone, Nil, Identifier(cl)))
        }

        val th = parts.head.split("\\|").toList.map(strToTH).reduceRight( THUnion )


        (parts.tail.mkString, th)
    }

    def extractTypeName(str: String): (String, TypeHint) = {
        extractType(str) match {
            case (str, th) =>
                val v = str.split("[^\\$a-zA-Z0-9_]", 2).toList.head;
                if (v.substring(0, 1) == "$") {
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

    def importFunctionsAnnotations(fd: FunctionDecl, comment: String): FunctionDecl = {
        // Here we import the annotations as type hints for functions
        val lines = comment.split("\n").toList
        FunctionDecl(fd.name, injectArgsHints(fd.args, paramsTH(lines)), fd.retref, returnTH(lines), fd.body).setPos(fd)
    }

    def importMethodsAnnotations(md: MethodDecl, comment: String): MethodDecl = {
        // Here we import the annotations as type hints for methods
        val lines = comment.split("\n").toList
        MethodDecl(md.name, md.flags, injectArgsHints(md.args, paramsTH(lines)), md.retref, returnTH(lines), md.body).setPos(md)
    }

    def importPropertiesAnnotations(pd: PropertyDecl, comment: String): PropertyDecl = {
        // Here we import the annotations as type hints for properties
        val lines = comment.split("\n").toList
        PropertyDecl(pd.v, pd.flags, pd.default, varTH(lines)).setPos(pd)
    }
}
