package phantm.util
import io.Source
import java.io.File

import scala.util.control.Breaks._

case class DumpCollector(path: String) {
    private val content = Source.fromFile(new File(path)).getLines("\n").toList

    var functions: Map[String, (String, Int)] = Map()

    breakable {
        for (l <- content.drop(3)) {
            if (l.startsWith("#")) break
            l.split(":", 3).toList match {
                case name :: line :: file :: Nil =>
                    functions += name -> (file, line.toInt)
                case _ =>
            }
        }
    }

    val heap: Unserializer = new Unserializer(content.drop(3+functions.size+1).reduceLeft(_ + "\n" + _))
}
