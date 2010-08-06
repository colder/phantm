package phantm.types;

import scala.util.control.Breaks._
import phantm.ast.{Trees => AST}
import phantm.cfg.{Trees => CFG}
import phantm.symbols._

object RecProtection {
    var objectToStringDepth = 0;
    var objectDepthDepth = 0;
}

sealed abstract class Type {
    self=>

    def equals(t: Type) = t == self;

    def depth(env: TypeEnvironment): Int = 1;

    def union(t: Type) = TypeLattice.join(this, t)
    def leq(t: Type) = TypeLattice.leq(this, t)

    def toText(e: TypeEnvironment): String = toString
    def toText: String = toText(BaseTypeEnvironment)
}

sealed abstract class ConcreteType extends Type;

// Classes types
sealed abstract class ClassType {
    def isSubtypeOf(cl2: ClassType): Boolean;
}
case object TAnyClass extends ClassType {
    def isSubtypeOf(cl2: ClassType) = false;
    override def toString = "?"
}

case class TClass(val cs: ClassSymbol) extends ClassType {
    override def toString = cs.name
    def isSubtypeOf(cl2: ClassType) = cl2 match {
        case TAnyClass => true
        case TClass(cs2) =>
            cs.subclassOf(cs2)
    }
}

// Functions types
sealed abstract class FunctionType {
    val ret: Type;
}
object TFunctionAny extends FunctionType {
    val ret = TAny
}

case class TFunction(val args: List[(Type, Boolean, Boolean)], val ret: Type) extends FunctionType {

    override def toString = args.map{a => a match {
            case (t, false, false) => t
            case (t, false, true) => "["+t+"]"
            case (t, true, false) => "&"+t
            case (t, true, true) => "[&"+t+"]"
        }}.mkString("(", ", ", ")")+" => "+ret
}

// Objects related types
sealed abstract class ObjectIdType;
case object ObjectIdUse extends ObjectIdType {
    override def toString = "use"
}

case class ObjectId(val pos: Int, val typ: ObjectIdType)

// Stores the ref => Real Objects relashionship

case class ObjectStore(val store: Map[ObjectId, TRealObject]) {

    def this() = this(Map[ObjectId, TRealObject]())

    /*
    def union(os: ObjectStore): ObjectStore = {
        var res = new ObjectStore()

        for (id <- this.store.keySet ++ os.store.keySet) {
            val c1 = this.store.contains(id);
            val c2 =   os.store.contains(id);

            if (c1 && c2) {
                res = res.set(id, this.store(id) merge os.store(id))
            } else if (c1) {
                res = res.set(id, this.store(id))
            } else {
                res = res.set(id, os.store(id))
            }
        }

        res
    }
    */

    def lookup(id: TObjectRef): TRealObject = lookup(id.id);

    def lookup(id: ObjectId): TRealObject = store.get(id) match {
        case Some(o) => o
        case None => error("Woops incoherent store")
    }

    def unset(id: ObjectId): ObjectStore = new ObjectStore(store - id);
    def set(id: ObjectId, robj: TRealObject): ObjectStore = new ObjectStore(store.updated(id, robj));

    def initIfNotExist(id: ObjectId, ocs: Option[ClassSymbol]) : ObjectStore = store.get(id) match {
        case Some(_) =>
            this
        case None =>
            set(id, newObject(id, ocs));
    }

    def newObject(id: ObjectId, ocs: Option[ClassSymbol]) : TRealObject = ocs match {
        case Some(cs) =>
            // construct a default object for this class
            new TRealObject(Map[String,Type]() ++ cs.properties.mapValues[Type] { x => x.typ }, TUninitialized, true, TClass(cs))
        case None =>
            // No class => any object
            new TRealObject(Map[String,Type](), TUninitialized, true, TAnyClass)
    }

    override def toString = {
        store.toList.sortWith{(x,y) => x._1.pos < x._1.pos}.map(x => "("+x._1.pos+","+x._1.typ+") => "+x._2).mkString("{ ", "; ", " }");
    }
}


// Object types exposed to symbols
sealed abstract class ObjectType extends ConcreteType

// Any object, should be only used to typecheck, no symbol should be infered to this type
object TAnyObject extends ObjectType {
    override def toString = "TAnyObject"
    override def toText(e: TypeEnvironment)   = "any object"
}

abstract class TPreciseObject extends ObjectType {
    def realObject(e: TypeEnvironment): TRealObject;

    override def depth(e: TypeEnvironment) = realObject(e).depth(e)
}

// Reference to an object in the store
class TObjectRef(val id: ObjectId) extends TPreciseObject {
    override def toString = "TObjectRef#"+id+""

    def realObject(e: TypeEnvironment) = e.store.lookup(id)

    override def toText(e: TypeEnvironment) = {
        e.store.store.get(id) match {
            case Some(o) => o.toText(e)
            case None => "Object(#"+id+")"
        }
    }

    override def equals(v: Any) = v match {
        case ref: TObjectRef =>
            ref.id == id
        case _ => false
    }

    override def hashCode = {
        id.pos*id.typ.hashCode
    }
}

class TObjectTmp(obj: TRealObject) extends TPreciseObject {
    override def toString = "TObjectTMP("+obj+")"

    override def toText(e: TypeEnvironment) = obj.toText(e)

    def realObject(e: TypeEnvironment) = obj
}


// Real object type (in the store) representing a specific object of any class
class TRealObject(val fields: Map[String, Type],
                  val globalType: Type,
                  val singleton: Boolean,
                  val ct: ClassType) {

    def copy(fields: Map[String, Type] = this.fields,
             globalType: Type = this.globalType,
             singleton: Boolean = this.singleton,
             ct: ClassType = this.ct): TRealObject = {
        
        new TRealObject(fields, globalType, singleton, ct)
    }

    override def equals(o: Any): Boolean = o match {
        case ro: TRealObject =>
            fields == ro.fields &&
            globalType == ro.globalType &&
            singleton == ro.singleton &&
            ct == ro.ct
        case _ =>
            false
    }

    def depth(e: TypeEnvironment) = {
        RecProtection.objectDepthDepth += 1;
        val res = if (RecProtection.objectDepthDepth > 5) {
            5
        } else {
            globalType.depth(e).max(fields.map(_._2.depth(e)).foldLeft(0)((a, b) => a.max(b)))
        }
        RecProtection.objectDepthDepth -= 1;

        res+1
    }

    def lookupField(index: CFG.SimpleValue): Type = index match {
      case CFG.PHPLong(i)       => lookupField(i+"")
      case CFG.PHPString(index) => lookupField(index)
      case _ => globalType
    }

    def lookupField(index: String) =
        fields.getOrElse(index, globalType)

    def lookupMethod(index: String, from: Option[ClassSymbol]): Option[MethodSymbol] = ct match {
        case TClass(cs) => 
            cs.lookupMethod(index, from) match {
                case LookupResult(Some(ms), _, _) =>
                    Some(ms)
                case LookupResult(None, _, _) =>
                    None
            }
        case TAnyClass =>
            None
    }

    def lookupMethod(index: CFG.SimpleValue, from: Option[ClassSymbol]): Option[MethodSymbol] = index match {
        case CFG.PHPLong(i)       => lookupMethod(i+"", from)
        case CFG.PHPString(index) => lookupMethod(index, from)
        case _ => None
    }

    def injectField(index: CFG.SimpleValue, typ: Type): TRealObject =
        injectField(index, typ, true)

    def injectField(index: CFG.SimpleValue, typ: Type, weak: Boolean): TRealObject = index match {
      case CFG.PHPLong(i)       => injectField(i+"",  typ, weak)
      case CFG.PHPString(index) => injectField(index, typ, weak)
      case _ => injectAnyField(typ)
    }

    def injectField(index: String, typ: Type): TRealObject =
        injectField(index, typ, true)


    def injectField(index: String, typ: Type, weak: Boolean): TRealObject = {
        val newFields = fields.updated(index, if (weak && !singleton) typ union lookupField(index) else typ)
        copy(fields = newFields)
    }

    def setMultiton = copy(singleton = false)

    def setSingleton = copy(singleton = true)

    // Used for type constructions
    def setAnyField(typ: Type) = copy(globalType = typ)

    def injectAnyField(typ: Type) = {
        var newFields = fields;
        // When the index is unknown, we have to pollute every entries
        for ((i,t) <- fields) {
            newFields = newFields.updated(i,t union typ)
        }

        copy(fields = newFields, globalType = globalType union typ)
    }


    override def toString = {
        RecProtection.objectToStringDepth += 1;
        var r = (if (singleton) "S" else "M")
        
        r += "Object("+ct+")"

        if (RecProtection.objectToStringDepth < 2) {
            r = r+"["+((fields.map(x => x._1 +" => "+ x._2).toList ::: "? -> "+globalType :: Nil).mkString("; "))+"]"
            ct match {
                case TClass(cs) =>
                    r = r+"["+(cs.methods.map(x => x._1+": "+x._2.ftyps.mkString("{", ",", "}")).mkString("; "))+"]"
                case _ =>
            }
        } else {
            r = r+"[...]"
        }
        RecProtection.objectToStringDepth -= 1;
        r
    }

    def toText(e: TypeEnvironment) = toString
}

object ArrayKey {
    def fromString(str: String): ArrayKey = {
        if (str.matches("^(-?[1-9][0-9]*|0)$")) {
            IntKey(str.toLong)
        } else {
            StringKey(str)
        }
    }
}

sealed abstract class ArrayKey {
    def < (o: ArrayKey) = (this, o) match {
        case (IntKey(v1), IntKey(v2)) => v1 < v2
        case (IntKey(v1), StringKey(_)) => true
        case (StringKey(v1), StringKey(v2)) => v1 < v2
        case _ => false
    }
}

case class StringKey(v: String) extends ArrayKey {
    override def toString = "\""+v+"\""
}
case class IntKey(v: Long) extends ArrayKey {
    override def toString = v.toString
}


class TArray(val entries: Map[ArrayKey, Type], val globalInt: Type, val globalString: Type) extends ConcreteType {

    val global = globalInt union globalString

    def this() =
        this(Map[ArrayKey, Type](), TUninitialized, TUninitialized)

    def this(global: Type) =
        this(Map[ArrayKey, Type](), global, global)

    def this(globalInt: Type, globalString: Type) =
        this(Map[ArrayKey, Type](), globalInt, globalString)

    def lookup(index: ArrayKey): Type = index match {
        case ik: IntKey =>
            entries.getOrElse(index, globalInt)
        case sk: StringKey => 
            entries.getOrElse(index, globalString)
    }

    def lookupByType(typ: Type): Type = typ match {
        case TIntLit(v) => lookup(IntKey(v))
        case TFloatLit(v) => lookup(IntKey(v.toLong))
        case TStringLit(v) => lookup(ArrayKey.fromString(v))
        case tu: TUnion =>
            (tu.types.map { lookupByType(_) }).reduceLeft(_ union _)

        case t if t leq TNumeric => globalInt
        case t if t leq TString  => globalString
        case _ => global
    }

    def injectByType(indtyp: Type, typ: Type): TArray = indtyp match {
        case TIntLit(v) => inject(IntKey(v), typ)
        case TFloatLit(v) => inject(IntKey(v.toLong), typ)
        case TStringLit(v) => inject(ArrayKey.fromString(v), typ)
        case tu: TUnion =>
            var globalChanges = Set[Type]()
            var resType = this

            // apply all precise changes
            for (t <- tu.types) {
                t match {
                    case TIntLit(v)    => resType = resType.inject(IntKey(v), typ, true)
                    case TFloatLit(v)  => resType = resType.inject(IntKey(v.toLong), typ, true)
                    case TStringLit(v) => resType = resType.inject(ArrayKey.fromString(v), typ, true)
                    case _ => globalChanges += t
                }
            }

            breakable {
                for (t <- globalChanges) {
                    if (t leq TNumeric) {
                        resType = resType.injectAnyInt(typ)
                    } else if (t leq TString) {
                        resType = resType.injectAnyString(typ)
                    } else {
                        resType = injectAny(typ)
                        break;
                    }
                } 
            }

            resType
        case t if t leq TNumeric => injectAnyInt(typ)
        case t if t leq TString  => injectAnyString(typ)
        case _ => injectAny(typ)
    }

    override def depth(env: TypeEnvironment): Int =
        globalInt.depth(env).max(globalString.depth(env).max(entries.map(_._2.depth(env)).foldLeft(0)(_ max _)))+1

    def inject(index: ArrayKey, typ: Type, maybe: Boolean = false): TArray = {
        if (maybe) {
            new TArray(entries + (index -> (lookup(index) union typ)), globalInt, globalString)
        } else {
            new TArray(entries + (index -> typ), globalInt, globalString)
        }
    }

    // used for type constructions
    def setAny(typ: Type): TArray = {
        new TArray(entries, typ, typ)
    }
    def setAnyInt(typ: Type): TArray = {
        new TArray(entries, typ, globalString)
    }
    def setAnyString(typ: Type): TArray = {
        new TArray(entries, globalInt, typ)
    }

    def injectAny(typ: Type): TArray = {
        // When the index is unknown, we have to pollute every entries
        var newEntries = Map[ArrayKey, Type]();
        for ((i,t) <- entries) {
            newEntries = newEntries + (i -> (t union typ))
        }

        new TArray(newEntries, globalInt union typ, globalString union typ)
    }

    def injectAnyString(typ: Type): TArray = {
        // When the index is unknown, we have to pollute every string entries
        var newEntries = Map[ArrayKey, Type]();
        for ((i,t) <- entries) {
            if (i.isInstanceOf[StringKey]) {
                newEntries = newEntries + (i -> (t union typ))
            } else {
                newEntries = newEntries + (i -> t)
            }
        }

        new TArray(newEntries, globalInt, globalString union typ)
    }

    def injectAnyInt(typ: Type): TArray = {
        // When the index is unknown, we have to pollute every int entries
        var newEntries = Map[ArrayKey, Type]();
        for ((i,t) <- entries) {
            if (i.isInstanceOf[IntKey]) {
                newEntries = newEntries + (i -> (t union typ))
            } else {
                newEntries = newEntries + (i -> t)
            }
        }

        new TArray(newEntries, globalInt union typ, globalString)
    }

    def merge(a2: TArray): TArray = {
        var newEntries = Map[ArrayKey, Type]()

        for (k <- a2.entries.keySet ++ entries.keySet) {
            newEntries = newEntries + (k -> (lookup(k) union a2.lookup(k)))
        }

        new TArray(newEntries, globalInt union a2.globalInt, globalString union a2.globalString)
    }

    override def equals(t: Any): Boolean = t match {
        case ta: TArray =>
            entries == ta.entries && globalInt == ta.globalInt && globalString == ta.globalString
        case _ => false
    }

    override def hashCode = {
        (entries.values.foldLeft(0)((a,b) => a ^ b.hashCode)) + globalString.hashCode + globalInt.hashCode
    }

    override def toText(env: TypeEnvironment) =
        "Array["+(entries.toList.sortWith((x,y) => x._1 < y._1).map(x => x._1 +" => "+ x._2.toText(env)).toList ::: "?s => "+globalString.toText(env) :: "?i => "+globalInt.toText(env) :: Nil).mkString(", ")+"]"

    override def toString =
        "Array["+(entries.toList.sortWith((x,y) => x._1 < y._1).map(x => x._1 +" => "+ x._2).toList ::: "?s => "+globalString :: "?i => "+globalInt :: Nil).mkString("; ")+"]"
}

object TAnyArray extends TArray(Map(), TTop, TTop) {
    override def toString = "Array[?]"
    override def toText(e: TypeEnvironment) = "Any array"

    override def equals(t: Any): Boolean = t match {
        case r: AnyRef =>
            this eq r
        case _ => false
    }
}

sealed abstract class TNumericLit extends ConcreteType

case object TNumeric extends ConcreteType {
    override def toText(e: TypeEnvironment) = "Numeric"
}

case object TInt extends ConcreteType {
    override def toText(e: TypeEnvironment) = "Int"
}

case class TIntLit(value: Long) extends TNumericLit {
    override def toText(e: TypeEnvironment) = "Int("+value+")"
}

case object TFloat extends ConcreteType {
    override def toText(e: TypeEnvironment) = "Float"
}

case class TFloatLit(value: Float) extends TNumericLit {
    override def toText(e: TypeEnvironment) = "Float("+value+")"
}

case object TBoolean extends ConcreteType {
    override def toText(e: TypeEnvironment) = "Boolean"
}
case object TTrue extends ConcreteType {
    override def toText(e: TypeEnvironment) = "True"
}
case object TFalse extends ConcreteType {
    override def toText(e: TypeEnvironment) = "False"
}

case object TString extends ConcreteType {
    override def toText(e: TypeEnvironment) = "String"
}

case class TStringLit(value: String) extends ConcreteType {
    override def toText(e: TypeEnvironment) = "String("+value+")"
}

case object TAny extends ConcreteType {
    override def toText(e: TypeEnvironment) = "Any"
}
case object TResource extends ConcreteType {
    override def toText(e: TypeEnvironment) = "Resource"
}
case object TNull extends ConcreteType {
    override def toText(e: TypeEnvironment) = "null"
}

/* Special types */
case object TTop extends Type {
    override def toText(e: TypeEnvironment) = "Top"
}

case object TBottom extends Type {
    override def toText(e: TypeEnvironment) = "Bottom"
}

case object TUninitialized extends Type {
    override def toText(e: TypeEnvironment) = "Uninitialized"
}

object TUnion {
    def apply(ts: Iterable[Type]) = {

        var tset = Set[Type]()

        for (t <- ts) {
            tset = addToSet(tset, t)
        }

        if (tset.size == 0) {
            TBottom
        } else if (tset.size == 1) {
            tset.toList.head
        } else {
            new TUnion(tset)
        }
    }

    def apply(t1: Type, t2: Type) = {
        if (t1 == t2) {
            t1
        } else {
            val s = getSet(t1, t2)
            s.size match {
                case 0 => TBottom
                case 1 => s.toList.head
                case _ => new TUnion(s)
            }
        }
    }

    def getSet(t1: Type, t2: Type) = (t1, t2) match {
        case (_, tu: TUnion) =>
            addToSet(tu.types, t1)
        case (tu: TUnion, _) =>
            addToSet(tu.types, t2)
        case (_, _) =>
            addToSet(Set[Type](t1), t2)
    }

    def addToSet(typs: Set[Type], typ: Type): Set[Type] = {
        val res: Set[Type] = typ match {
            case tu: TUnion =>
                var res = typs;
                for (t <- tu.types if !(res contains t)) {
                    res = addToSet(res, t)
                }
                res

            case TString =>
                typs.filter(!_.isInstanceOf[TStringLit]) + TString

            case t: TStringLit =>
                if (typs contains TString) {
                    typs
                } else {
                    typs + t
                }

            case TNumeric =>
                typs.filter(t => (t != TInt) && (t != TFalse) && (!t.isInstanceOf[TNumericLit])) + TNumeric

            case TInt =>
                if (typs contains TFloat) {
                    addToSet(typs, TNumeric)
                } else if (typs contains TNumeric) {
                    typs
                } else {
                    typs.filter(t => !t.isInstanceOf[TIntLit]) + TInt
                }

            case TFloat =>
                if (typs contains TInt) {
                    addToSet(typs, TNumeric)
                } else if (typs contains TNumeric) {
                    typs
                } else {
                    typs.filter(t => !t.isInstanceOf[TFloatLit]) + TFloat
                }

            case t: TIntLit =>
                if ((typs contains TInt) || (typs contains TNumeric)) {
                    typs
                } else {
                    typs + t
                }

            case t: TFloatLit =>
                if ((typs contains TFloat) || (typs contains TNumeric)) {
                    typs
                } else {
                    typs + t
                }

            case TBoolean =>
                typs.filter(t => (t != TFalse) && (t != TTrue)) + TBoolean

            case TFalse =>
                if (typs contains TTrue) {
                    addToSet(typs, TBoolean)
                } else if (typs contains TBoolean) {
                    typs
                } else {
                    typs + TFalse
                }
            case TTrue =>
                if (typs contains TFalse) {
                    addToSet(typs, TBoolean)
                } else if (typs contains TBoolean) {
                    typs
                } else {
                    typs + TTrue
                }
            case TAnyArray =>
                typs.filter(t => ! t.isInstanceOf[TArray]) + TAnyArray

            case ta: TArray =>
                if (typs contains TAnyArray) {
                    typs
                } else {
                    // if the union contains an array, we need to merge the two arrays
                    val oar = typs.find(_.isInstanceOf[TArray])
                    oar match {
                        case Some(ar: TArray) =>
                            (typs - ar) + (ar merge ta)
                        case Some(ar) =>
                            println("Woops, incoherent find")
                            typs
                        case None =>
                            typs + ta
                    }
                }
            case typ =>
                typs + typ
        }

        for (t <- res) t match {
            case _: TUnion =>
                println("WOOOOOOOOOOT: addToList("+typs+", "+typ+") includes TUnion!")
            case _ =>
        }

        res
    }
}

class TUnion(val types: Set[Type]) extends Type {

    override def equals(t: Any): Boolean = t match {
        case tu: TUnion =>
            this.types == tu.types
        case _ => false
    }

    override def depth(env: TypeEnvironment) =
        types.map(_.depth(env)).reduceLeft(_ max _)

    override def toString = types.mkString("{", ",", "}")
    override def toText(e: TypeEnvironment)   = types.map(t => t.toText(e)).mkString(" or ")

    override def hashCode = {
        (types.foldLeft(0)((a,b) => a ^ b.hashCode))
    }

    if (types.size < 2) throw new RuntimeException("TUnion should at least be 2 types!")
}
