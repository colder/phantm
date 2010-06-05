package phantm.annotations
import phantm.ast._
import phantm.types._
import phantm.symbols._
import phantm.util.Reporter
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object SourceAnnotations {
    object Parser extends StandardTokenParsers {
        lexical.delimiters += ("[", "]", ",", "=>", "$", "?", "|", "#", "=")
        lexical.reserved += ("array", "Array")

        var typedefs = Map[String, Type]();

        def arrentriesToArray(aes: List[(Option[String], Type)]): TArray = {
            var global: Type = TTop
            var entries = Map[String, Type]()

            for ((os, t) <- aes) {
                if (os.isEmpty) {
                    global = t
                } else {
                    entries += (os.get -> t)
                }
            }

            new TArray(entries, global)

        }

        def identToType(name: String): Type = name.toLowerCase match {
            case "string" => TString
            case "mixed" => TAny
            case "long" | "int" | "integer" => TInt
            case "true" => TTrue
            case "false" => TFalse
            case "bool" | "boolean" => TBoolean
            case "null" => TNull
            case "float" | "double" => TFloat
            case "number" | "numeric" => TNumeric
            case "undef" | "uninit" => TUninitialized
            case "top" => TTop
            case "bottom" => TBottom
            case "void" => TNull
            case "object" => TAnyObject
            case "resource" => TResource
            case _ => TAny
        }

        def array: Parser[TArray] =
            "array" ~ "[" ~> repsep(arrayentry, ",") <~ "]" ^^ { case aes => arrentriesToArray(aes) } |
            "Array" ~ "[" ~> repsep(arrayentry, ",") <~ "]" ^^ { case aes => arrentriesToArray(aes) } |
            "array" ^^^ TAnyArray
            "Array" ^^^ TAnyArray

        def arrayentry: Parser[(Option[String], Type)] =
            stringLit  ~ "=>" ~ typ ^^ { case key ~ "=>" ~ typ => (Some(key.toString), typ) } |
            numericLit ~ "=>" ~ typ ^^ { case key ~ "=>" ~ typ => (Some(key.toString), typ) } |
            "?" ~ "=>" ~> typ ^^ ((None, _: Type))

        def typ: Parser[Type] =
            ident ^^ { i => identToType(i.toString) } |
            array |
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
            utyp ~ variable ^^ { case t ~ v => (v, t) } |
            variable ~ utyp ^^ { case v ~ t => (v, t) }


        // Parsing helpers
        def filterLines(comment: String, tag: String): List[String] =
            filterLines(comment.split("\n").toList, tag)

        def filterLines(lines: List[String], tag: String): List[String] = {
            lines.filter(_.indexOf(tag+" ") >= 0).map(s => s.substring(s.indexOf(tag+" ")+tag.length+1))
        }

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

        def importTypeDefs(comment: String): Unit = {
            for (l <- filterLines(comment, "@typedef")) {
                importTypeDef(l)
            }
        }

        def getAnyType(tag: String)(comment: String): Option[Type] = {
            var ret: Option[Type] = None

            for (l <- filterLines(comment, tag)) {
                strToType(l) match {
                    case Some(r) => ret = Some(r)
                    case None =>
                }
            }

            ret
        }

        def getReturnType = getAnyType("@return")_
        def getVarType = getAnyType("@var")_
        def getConstType = getAnyType("@const")_

        def isAnnotated(comment: String): Boolean = {
            (getReturnType(comment) != None) || (!filterLines(comment, "@param").isEmpty)
        }

        def shouldInline(comment: String): Boolean = {
            !filterLines(comment, "@inline").isEmpty
        }

        def getFunctionTypes(comment: String): (Map[String, Type], Type) = {
            var args = Map[String, Type]()

            for (l <- filterLines(comment, "@param")) {
                strToVarType(l) match {
                    case Some(r) => args += r
                    case None =>
                }
            }

            (args, getReturnType(comment).getOrElse(TAny))
        }

        def importTypeDef(line: String): Unit = {
            val s = new lexical.Scanner(line)
            val r = typedef(s)
            if (!r.isEmpty) {
                val (name, t) = r.get
                typedefs += (name -> t)
            }
        }

    }

}
