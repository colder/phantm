package phantm.annotations
import phantm.ast._
import phantm.types._
import phantm.symbols._
import phantm.util.Reporter
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object SourceAnnotations {
    object Parser extends StandardTokenParsers {
        lexical.delimiters += ("[", "]", ",", "=>", "$", "?", "?s", "?i",  "|", "#", "=")
        lexical.reserved += ("array", "Array")

        var typedefs = Map[String, Type]();

        def arrentriesToArray(aes: List[ArrayEntryType]): TArray = {
            var globalInt: Type = TTop
            var globalString: Type = TTop
            var entries = Map[ArrayKey, Type]()

            for (ae <- aes) ae match {
                case Entry(s, t) =>
                    entries += (ArrayKey.fromString(s) -> t)
                case AnyEntry(t) =>
                    globalInt = t
                    globalString = t
                case AnyStringEntry(t) =>
                    globalString = t
                case AnyIntEntry(t) =>
                    globalInt = t
            }

            new TArray(entries, globalInt, globalString)

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

        sealed abstract class ArrayEntryType;
        case class AnyEntry(t: Type) extends ArrayEntryType
        case class AnyStringEntry(t: Type) extends ArrayEntryType
        case class AnyIntEntry(t: Type) extends ArrayEntryType
        case class Entry(s: String, t: Type) extends ArrayEntryType

        def arrayentry: Parser[ArrayEntryType] =
            stringLit  ~ "=>" ~ typ ^^ { case key ~ "=>" ~ typ => Entry(key.toString, typ) } |
            numericLit ~ "=>" ~ typ ^^ { case key ~ "=>" ~ typ => Entry(key.toString, typ) } |
            "?s" ~ "=>" ~> typ ^^ (t => AnyStringEntry(t)) |
            "?i" ~ "=>" ~> typ ^^ (t => AnyIntEntry(t)) |
            "?" ~ "=>" ~> typ ^^ (t => AnyEntry(t))

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

        def contains(comment: String, tag: String): Boolean = {
            comment.indexOf(tag) >= 0
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
            contains(comment, "@inline")
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
