package phantm.util
import io.{Source, Codec}
import java.io.File
import java.nio.charset.Charset

import phantm.phases.PhasesContext

import scala.util.control.Breaks._

case class DumpCollector(path: String, ctx: PhasesContext) {
    private val content = Source.fromFile(new File(path))(new Codec(Charset.forName("ISO-8859-1"))).getLines.toList
    var lineNr = 3

    var files: List[String] = Nil
    var functions: Map[String, (String, Int)] = Map()
    var classes: Map[String, (String, Int)]   = Map()

    def restore(str: String): String = {
        var res = new java.lang.StringBuffer(str.length)

        var arr = str.toArray

        var i = 0;

        while(i < arr.length) {
            if (arr(i) == '\\') {
                i += 1

                if (arr(i) == 'n') {
                    res.append('\n')
                } else if (arr(i) == 'r') {
                    res.append('\r')
                } else {
                    res.append('\\')
                }
            } else {
                res.append(arr(i))
            }
            i += 1
        }

        res.toString
    }

    breakable {
        for (l <- content.drop(lineNr)) {
            if (l.startsWith("#")) break
            l.split(":", 2).toList match {
                case fmtime :: file :: Nil =>
                    // Check timestamp against the file
                    val f = new java.io.File(file)
                    val t = f.lastModified/1000
                    if (t > fmtime.toLong) {
                        Reporter.notice("File '"+file+"' modified after dumping, could result is mismatched line numbers");
                    }
                    files = file :: files
                case _ =>
            }
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

    val constants: Unserializer = new Unserializer(restore(content(lineNr)), ctx)
    val heap: Unserializer      = new Unserializer(restore(content(lineNr+2)), ctx)
}
