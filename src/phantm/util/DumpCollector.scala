package phantm.util
import io.{Source, Codec}
import java.io.File
import java.nio.charset.Charset

import scala.util.control.Breaks._

case class DumpCollector(path: String) {
    private val content = Source.fromFile(new File(path))(new Codec(Charset.forName("ISO-8859-1"))).getLines("\n").toList
    var lineNr = 3

    var files: List[String] = Nil
    var functions: Map[String, (String, Int)] = Map()
    var classes: Map[String, (String, Int)]   = Map()

    def restore(str: String): String = {
        var res = ""

        var arr = str.toList

        while(arr != Nil) arr match {
            case '\\' :: '\\' :: xs =>
                arr = xs
                res += '\\'
            case '\\' :: 'n' :: xs =>
                arr = xs
                res += '\n'
            case c :: xs =>
                arr = xs
                res += c
            case Nil =>
        }

        res
    }

    breakable {
        for (l <- content.drop(lineNr)) {
            if (l.startsWith("#")) break
            files = l :: files
        }
    }

    lineNr += files.size + 1

    breakable {
        for (l <- content.drop(lineNr)) {
            if (l.startsWith("#")) break
            l.split(":", 3).toList match {
                case name :: line :: file :: Nil =>
                    functions += name -> (file, line.toInt)
                case _ =>
            }
        }
    }

    lineNr += functions.size + 1

    breakable {
        for (l <- content.drop(lineNr)) {
            if (l.startsWith("#")) break
            l.split(":", 3).toList match {
                case name :: line :: file :: Nil =>
                    classes += name -> (file, line.toInt)
                case _ =>
            }
        }
    }

    lineNr += classes.size + 1

    val constants: Unserializer = new Unserializer(restore(content(lineNr)))
    val heap: Unserializer      = new Unserializer(restore(content(lineNr+2)))
}
