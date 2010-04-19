package phantm.analyzer
import phantm.parser.Trees._
import Types._
import Symbols._
import phantm.Reporter
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object SourceAnnotations {
    object Parser extends StandardTokenParsers {
        lexical.delimiters += ("[", "]", ",", "=>", "$", "?", "|", "#", "=")
        lexical.reserved   += ("string", "mixed", "long", "int", "false", "true", "null",
                               "number", "integer", "float", "double", "array", "object",
                               "resource", "bool", "boolean", "void", "numeric", "top",
                               "undef", "uninit")

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
            "top" ^^^ TTop |
            "undef" ^^^ TUninitialized |
            "uninit" ^^^ TUninitialized |
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

        def getAnyType(tag: String)(comment: String): Type = {
            var ret: Type = TAny

            for (l <- filterLines(comment, tag)) {
                strToType(l) match {
                    case Some(r) => ret = r
                    case None =>
                }
            }

            ret
        }

        def getReturnType = getAnyType("@return")_
        def getVarType = getAnyType("@var")_

        def getFunctionTypes(comment: String): (Map[String, Type], Type) = {
            var args = Map[String, Type]()

            for (l <- filterLines(comment, "@param")) {
                strToVarType(l) match {
                    case Some(r) => args += r
                    case None =>
                }
            }

            (args, getReturnType(comment))
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
