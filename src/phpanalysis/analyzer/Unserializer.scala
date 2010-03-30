package phpanalysis.analyzer;
import Symbols._
import Types._
import io.Source
import java.io.File

class UnserializeException(msg: String) extends Exception(msg)


object Unserializer {
    sealed abstract class UValue;

    case class UArray(entries: Map[UValue, UValue]) extends UValue
    case class UObject(classname: String, entries: Map[UValue, UValue]) extends UValue
    case class UString(str: String) extends UValue
    case class UInt(str: Int) extends UValue
    case class UFloat(str: Float) extends UValue
    case class UObjRef(i: Int) extends UValue
    case class URealRef(i: Int) extends UValue

    var pos = 0;
    
    def fromDump(path: String): Option[Map[String, Type]] = {
        try {
            val content = Source.fromFile(new File(path)).getLines
            val r = unserialize(content.drop(2).reduceLeft(_+_))
            println(r);
            None
        } catch {
            case e: Exception =>
                e.printStackTrace
                None
        }
    }

    def unserialize(str: String): UValue = unserialize(str.toList)

    def unserialize(charsInit: List[Char]): UValue = {
        var chars = charsInit

        var valueStore : List[UValue] = Nil

        def updateVal(v: UValue, pos: Int): UValue = {
            valueStore = valueStore.take(pos) ::: List(v) ::: valueStore.drop(pos+1)
            v
        }

        def regVal(v: UValue): UValue = {
            valueStore = valueStore ::: List(v)
            v
        }

        def getVal(i: Int): UValue = {
            valueStore(i-1)
        }

        def consumeInt : Int = {
            var buf = "";
            while(chars.head >= '0' && chars.head <= '9') {
                buf += chars.head
                chars = chars.tail
            }

            buf.toInt
        }

        def consumeFloat : Float = {
            var buf = "";
            while(chars.head >= '0' && chars.head <= '9' || chars.head == '.') {
                buf += chars.head
                chars = chars.tail
            }

            buf.toFloat
        }

        def consumeString(size: Int) : String = {
            val str = chars.tail.take(size)
            chars = chars.drop(size+2) // for the quotes
            str.mkString
        }

        def unser: UValue = {
            chars match {
                case 'i' :: ':' :: cs =>
                    chars = cs
                    val v = consumeInt
                    chars = chars.tail // consume the ;
                    regVal(UInt(v))
                case 'd' :: ':' :: cs =>
                    chars = cs
                    val v = consumeFloat
                    chars = chars.tail // consume the ;
                    regVal(UFloat(v))
                case 's' :: ':' :: cs =>
                    // s:<size>:"...";
                    chars = cs
                    val size = consumeInt
                    chars = chars.tail // consume the :
                    val str = consumeString(size)
                    chars = chars.tail // consume the ;
                    regVal(UString(str))
                case 'O' :: ':' :: cs =>
                    var map = Map[UValue, UValue]();
                    chars = cs
                    val cnsize = consumeInt
                    chars = chars.tail // consume the :
                    val cname = consumeString(cnsize)
                    chars = chars.tail // consume the :
                    val size = consumeInt
                    chars = chars.drop(2) // consume the extra ":{"
                    for (e <- 1 to size) {
                        val k = unser
                        val v = regVal(unser)
                        map += (k -> v)
                    }
                    chars = chars.tail // consume the extra "}"

                    regVal(UObject(cname, map))
                case 'a' :: ':' :: cs =>
                    var map = Map[UValue, UValue]();
                    chars = cs
                    val size = consumeInt
                    chars = chars.drop(2) // consume the extra ":{"
                    for (e <- 1 to size) {
                        val k = unser
                        val v = regVal(unser)
                        map += (k -> v)
                    }
                    chars = chars.tail // consume the extra "}"

                    regVal(UArray(map))
                case 'r' :: ':' :: cs =>
                    chars = cs
                    val r = consumeInt;
                    chars = chars.tail // consume the extra ";"
                    println("Object ref pointing to:"+getVal(r))
                    regVal(UObjRef(r))
                case 'R' :: ':' :: cs =>
                    chars = cs
                    val r = consumeInt;
                    chars = chars.tail // consume the extra ";"
                    println("Real ref pointing to:"+getVal(r))
                    regVal(URealRef(r))
                case c =>
                    throw new UnserializeException("Invalid serialized sequence: '"+c.mkString+"'")
            }
        }

        unser
    }
}
