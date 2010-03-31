package phpanalysis.analyzer;
import Symbols._
import Types._
import io.Source
import java.io.File
import controlflow.TypeFlow._
import controlflow.CFGTrees.CFGIdentifier

class UnserializeException(msg: String) extends Exception(msg)

sealed abstract class UValue;

case class UArray(entries: Map[UValue, UValue]) extends UValue
case class UObject(classname: String, entries: Map[UValue, UValue]) extends UValue
case class UString(str: String) extends UValue
case class UInt(str: Int) extends UValue
case class UFloat(str: Float) extends UValue
case class UObjRef(i: Int) extends UValue
case class URealRef(i: Int) extends UValue

object Unserializer {
    def fromDump(path: String): Unserializer = {
        val content = Source.fromFile(new File(path)).getLines
        new Unserializer(content.drop(2).reduceLeft(_+_))
    }
}

class Unserializer(content: String) {
    var valueStore : List[UValue] = Nil
    var chars = content.toList
    var result: UValue = unser(true)

    def updateVal(pos: Int, v: UValue): UValue = {
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

    //def importToEnv(env: TypeEnvironment): TypeEnvironment = {
    def importToEnv(envInit: TypeEnvironment): TypeEnvironment = {
        def uValueToKey(v: UValue) = v match {
            case UInt(i) => i+""
            case UString(str) => str
            case _ =>
                throw new UnserializeException("Invalid key value: "+v)
        }

        var recursionLimit = 0

        def uValueToType(v: UValue): Type = v match {
            case UInt(i) => TInt
            case UString(i) => TString
            case UFloat(i) => TFloat
            case UObject(cl, entries) => TAnyObject
            case UArray(entries) =>
                var res = Map[String, Type]()

                for ((k, v) <- entries) {
                    res += (uValueToKey(k) -> uValueToType(v))
                }

                new TArray(res, TUninitialized)

            case UObjRef(i) =>
                recursionLimit += 1
                val t = if (recursionLimit >= 5) {
                    TTop
                } else {
                    uValueToType(getVal(i))
                }
                recursionLimit -= 1

                t
            case URealRef(i) =>
                recursionLimit += 1
                val t = if (recursionLimit >= 5) {
                    TTop
                } else {
                    uValueToType(getVal(i))
                }
                recursionLimit -= 1

                t

        }


        result match {
            case UArray(entries) =>
                var env = envInit
                for ((k, v) <- entries) {
                    val key = uValueToKey(k)
                    val sym = GlobalSymbols.lookupVariable(key) match {
                        case Some(vs) =>
                            vs
                        case None =>
                            val vs = new VariableSymbol(key)
                            GlobalSymbols.registerVariable(vs)
                            vs
                    }

                    env = env.inject(CFGIdentifier(sym), uValueToType(v))
                }
                env
            case _ =>
                throw new UnserializeException("Invalid non-array input for dumpanddie")
        }
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

    def unser(r: Boolean): UValue = {
        chars match {
            case 'i' :: ':' :: cs =>
                chars = cs
                val v = consumeInt
                chars = chars.tail // consume the ;
                if(r) regVal(UInt(v)) else UInt(v)
            case 'd' :: ':' :: cs =>
                chars = cs
                val v = consumeFloat
                chars = chars.tail // consume the ;
                if (r) regVal(UFloat(v)) else UFloat(v)
            case 's' :: ':' :: cs =>
                // s:<size>:"...";
                chars = cs
                val size = consumeInt
                chars = chars.tail // consume the :
                val str = consumeString(size)
                chars = chars.tail // consume the ;
                if (r) regVal(UString(str)) else UString(str)
            case 'O' :: ':' :: cs =>
                var map = Map[UValue, UValue]();
                chars = cs
                val cnsize = consumeInt
                chars = chars.tail // consume the :
                val cname = consumeString(cnsize)
                chars = chars.tail // consume the :

                regVal(UObject(cname, map))
                val i = valueStore.size-1

                val size = consumeInt
                chars = chars.drop(2) // consume the extra ":{"
                for (e <- 1 to size) {
                    val k = unser(false)
                    val v = unser(true)
                    map += (k -> v)
                }
                chars = chars.tail // consume the extra "}"

                updateVal(i, UObject(cname, map))
            case 'a' :: ':' :: cs =>
                var map = Map[UValue, UValue]();

                regVal(UArray(map))
                val i = valueStore.size-1;

                chars = cs
                val size = consumeInt
                chars = chars.drop(2) // consume the extra ":{"
                for (e <- 1 to size) {
                    val k = unser(false)
                    val v = unser(true)
                    map += (k -> v)
                }
                chars = chars.tail // consume the extra "}"

                updateVal(i, UArray(map))
            case 'r' :: ':' :: cs =>
                chars = cs
                val r = consumeInt;
                chars = chars.tail // consume the extra ";"
                regVal(UObjRef(r))
            case 'R' :: ':' :: cs =>
                chars = cs
                val r = consumeInt;
                chars = chars.tail // consume the extra ";"
                regVal(URealRef(r))
            case c =>
                throw new UnserializeException("Invalid serialized sequence: '"+c.mkString+"'")
        }
    }
}
