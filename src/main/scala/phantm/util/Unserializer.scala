package phantm.util

import phantm.symbols._
import phantm.types._
import phantm.phases.PhasesContext
import phantm.ast.Trees.{Scalar, PHPInteger, PHPString, PHPNull, PHPTrue, PHPFalse, PHPFloat}
import phantm.cfg.Trees.Identifier

class UnserializeException(msg: String) extends Exception(msg)

sealed abstract class UValue;

case class UArray(var entries: Map[UValue, UValue]) extends UValue
case class UObject(i: Int, classname: String, var entries: Map[UValue, UValue]) extends UValue
case class UString(str: String) extends UValue
case class UInt(v: Int) extends UValue
case class UFloat(str: Float) extends UValue
case class UObjRef(i: Int) extends UValue
case class URealRef(i: Int) extends UValue
case object UNull extends UValue
case object UFalse extends UValue
case object UTrue extends UValue

class Unserializer(content: String, ctx: PhasesContext) {
    // allocate the first for the outer array
    var valueStore : List[UValue] = List(UNull)
    var uidToOId: Map[Int, ObjectId] = Map()
    var objectsStore: Map[ObjectId, TRealObject] = Map()

    var chars = content.toList
    var result: UValue = unser(true)

    def regVal(v: UValue): UValue = {
        valueStore = valueStore ::: List(v)
        v
    }

    def getVal(i: Int): UValue = {
        valueStore(i)
    }

    def uValueToField(v: UValue) = v match {
        case UString(str) => str
        case _ =>
            throw new UnserializeException("Invalid field value: "+v)
    }

    def uValueToKey(v: UValue) = v match {
        case UInt(i) => IntKey(i)
        case UString(str) => ArrayKey.fromString(str)
        case _ =>
            throw new UnserializeException("Invalid key value: "+v)
    }

    var recursionLimit = 0

    def uValueToScalar(v: UValue): Scalar = v match {
        case UFalse => PHPFalse()
        case UTrue => PHPTrue()
        case UNull => PHPNull()
        case UInt(i) => PHPInteger(i)
        case UString(v) => PHPString(v)
        case UFloat(f) => PHPFloat(f)
        case _ => throw new UnserializeException("Invalid serialized scalar value: "+v)
    }

    def uValueToType(v: UValue): Type = v match {
        case UFalse => TFalse
        case UTrue => TTrue
        case UNull => TNull
        case UInt(i) => TIntLit(i)
        case UString(v) => TStringLit(v)
        case UFloat(f) => TFloatLit(f)
        case UObject(i, cl, entries) =>
            uidToOId.get(i) match {
                case Some(id) =>
                    new TObjectRef(id)
                case None =>
                    val id = new ObjectId(0, ObjectIdDump(i))
                    uidToOId += i -> id

                    val ct = ctx.globalSymbols.lookupClass(cl) match {
                        case Some(cs) =>
                            new TClass(cs)
                        case None =>
                            TAnyClass
                    }

                    var res = Map[String, Type]()

                    for ((k, v) <- entries) {
                        res += (uValueToField(k) -> uValueToType(v))
                    }

                    val ro = new TRealObject(res, TUninitialized, true, ct)

                    objectsStore += id -> ro

                    new TObjectRef(id)
            }


        case UArray(entries) =>
            var res = Map[ArrayKey, Type]()

            for ((k, v) <- entries) {
                res += (uValueToKey(k) -> uValueToType(v))
            }

            new TArray(res, TUninitialized, TUninitialized)

        case UObjRef(i) =>
            uidToOId.get(i) match {
                case Some(oid) =>
                    new TObjectRef(oid)
                case _ =>
                    throw new UnserializeException("Invalid object reference: "+v)
            }

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

    //def importToEnv(env: TypeEnvironment): TypeEnvironment = {
    def importToEnv(envInit: TypeEnvironment): TypeEnvironment = {
        var env = envInit
        for ((k, t) <- toMap.mapValues(uValueToType _)) {
            val sym = ctx.globalSymbols.lookupVariable(k) match {
                case Some(vs) =>
                    vs
                case None =>
                    val vs = new VariableSymbol(k)
                    ctx.globalSymbols.registerVariable(vs)
                    vs
            }

            env = env.inject(Identifier(sym), t)
        }

        var st = env.store
        for ((id, ro) <- objectsStore) {
            st = st.set(id, ro)
        }
        env = env.setStore(st)

        env
    }

    def toMap = {
        var map = Map[String, UValue]()
        result match {
            case UArray(entries) =>
                for ((k, v) <- entries) {
                    uValueToKey(k) match {
                        case StringKey(str) =>
                            map += str -> v
                        case _ =>
                    }
                }
            case _ =>
                throw new UnserializeException("Invalid non-array base input found in dump")
        }
        map
    }

    def toScalarMap = toMap.mapValues(uValueToScalar _)

    def toTypeMap = toMap.mapValues(uValueToType _)


    def consumeInt : Int = {
        var buf = "";
        if (chars.head == '-') {
            buf += chars.head
            chars = chars.tail
        }

        while((chars.head >= '0' && chars.head <= '9')) {
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
            case 'N' :: ';' :: cs =>
                chars = cs
                regVal(UNull)
            case 'b' :: ':' :: cs =>
                chars = cs
                val v = consumeInt
                chars = chars.tail // consume the ;
                regVal(if (v > 0) UTrue else UFalse)
            case 'i' :: ':' :: cs =>
                chars = cs
                val v = consumeInt
                chars = chars.tail // consume the ;
                if(r) regVal(UInt(v)) else UInt(v)
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
                if (r) regVal(UString(str)) else UString(str)
            case 'O' :: ':' :: cs =>
                var map = Map[UValue, UValue]();
                chars = cs
                val cnsize = consumeInt
                chars = chars.tail // consume the :
                val cname = consumeString(cnsize)
                chars = chars.tail // consume the :

                val obj = UObject(valueStore.size, cname, map)
                regVal(obj)

                val size = consumeInt
                chars = chars.drop(2) // consume the extra ":{"
                for (e <- 1 to size) {
                    val k = unser(false)
                    val v = unser(true)
                    map += (k -> v)
                }
                chars = chars.tail // consume the extra "}"

                obj.entries = map
                obj
            case 'a' :: ':' :: cs =>
                var map = Map[UValue, UValue]();

                val arr = UArray(map)
                regVal(arr)

                chars = cs
                val size = consumeInt
                chars = chars.drop(2) // consume the extra ":{"
                for (e <- 1 to size) {
                    val k = unser(false)
                    val v = unser(true)
                    map += (k -> v)
                }
                chars = chars.tail // consume the extra "}"

                arr.entries = map
                arr
            case 'r' :: ':' :: cs =>
                chars = cs
                val r = consumeInt;
                chars = chars.tail // consume the extra ";"
                regVal(UObjRef(r))
            case 'R' :: ':' :: cs =>
                chars = cs
                val r = consumeInt;
                chars = chars.tail // consume the extra ";"
                URealRef(r)
            case c =>
                throw new UnserializeException("Invalid serialized sequence: '"+c.mkString+"'")
        }
    }
}
