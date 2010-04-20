package phantm.analyzer;
import Symbols._
import scala.collection.immutable.{Map, Set}

import phantm.lattice.TypeLattice
import phantm.controlflow.TypeEnvironment
import phantm.AST.{Trees => AST}
import phantm.CFG.{Trees => CFG}

object Types {
    object defaultType extends TypeWideningFunction {
        def apply(t: Type) = t match {
            case TNull => TAny
            case TIntLit(i) => TInt
            case TStringLit(s) => TString
            case TFloatLit(f) => TFloat
            case _ => t
        }
    }

    object RecProtection {
        var objectToStringDepth = 0;
        var objectDepthDepth = 0;
    }

    sealed abstract class Type {
        self=>

        def equals(t: Type) = t == self;

        def depth(env: TypeEnvironment): Int = 1;

        def union(t: Type) = TypeLattice.join(this, t)

        def toText(e: TypeEnvironment) = toString
    }

    sealed abstract class ConcreteType extends Type;

    // Classes types
    sealed abstract class ClassType {
        def isSubtypeOf(cl2: ClassType): Boolean;
    }
    case object TClassAny extends ClassType {
        def isSubtypeOf(cl2: ClassType) = true;
    }

    class TClass(val cs: ClassSymbol) extends ClassType {
        override def toString = cs.name
        def isSubtypeOf(cl2: ClassType) = cl2 match {
            case TClassAny => false
            case tc: TClass =>
                cs.subclassOf(tc.cs)
        }
    }

    // Functions types
    sealed abstract class FunctionType {
        val ret: Type;
    }
    object TFunctionAny extends FunctionType {
        val ret = TAny
    }

    case class TFunction(val args: List[(Type, Boolean)], val ret: Type) extends FunctionType {

        override def toString = args.map{a => a match {
                case (t, false) => t
                case (t, true) => "["+t+"]"
            }}.mkString("(", ", ", ")")+" => "+ret
    }

    // Objects related types
    case class ObjectId(val pos: Int, val offset: Int)

    // Stores the ref => Real Objects relashionship

    case class ObjectStore(val store: Map[ObjectId, TRealObject]) {

        def this() = this(Map[ObjectId, TRealObject]())

        def union(os: ObjectStore) : ObjectStore = {
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
                // We create a new object and place it in the store
                val rot = ocs match {
                    case Some(cs) =>
                        // construct a default object for this class
                        new TRealClassObject(new TClass(cs), Map[String,Type]() ++ cs.properties.mapValues[Type] { x => x.typ }, TUninitialized)
                    case None =>
                        // No class => any object
                        new TRealObject(Map[String,Type](), TUninitialized)
                }

                set(id, rot);
        }

        override def toString = {
            store.toList.sortWith{(x,y) => x._1.pos < x._1.pos}.map(x => "("+x._1.pos+","+x._1.offset+") => "+x._2).mkString("{ ", "; ", " }");
        }
    }


    // Object types exposed to symbols
    abstract class ObjectType extends ConcreteType

    // Any object, should be only used to typecheck, no symbol should be infered to this type
    object TAnyObject extends ObjectType {
        override def toString = "TAnyObject"
        override def toText(e: TypeEnvironment)   = "any object"
    }
    // Reference to an object in the store
    class TObjectRef(val id: ObjectId) extends ObjectType {
        override def toString = {
            "TObjectRef#"+id+""
        }

        def realObject(e: TypeEnvironment) = e.store.lookup(id)

        override def toText(e: TypeEnvironment) = {
            e.store.lookup(id).toText(e)
        }

        override def depth(e: TypeEnvironment) = realObject(e).depth(e)

        override def equals(v: Any) = v match {
            case ref: TObjectRef =>
                ref.id == id
            case _ => false
        }

        override def hashCode = {
            id.pos*id.offset
        }
    }

    // Real object type (in the store) representing a specific object of any class
    class TRealObject(val fields: Map[String, Type],
                      val globalType: Type) {

        override def equals(o: Any): Boolean = o match {
            case ro: TRealObject =>
                fields == ro.fields && globalType == ro.globalType
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
          case CFG.PHPLong(i)        => lookupField(i+"")
          case CFG.PHPString(index) => lookupField(index)
          case _ => globalType
        }

        def lookupField(index: String) =
            fields.getOrElse(index, globalType)

        def lookupMethod(index: String, from: Option[ClassSymbol]): Option[FunctionType] = None

        def lookupMethod(index: CFG.SimpleValue, from: Option[ClassSymbol]): Option[FunctionType] = index match {
            case CFG.PHPLong(i)        => lookupMethod(i+"", from)
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
            val newFields = fields.updated(index, if (weak) typ union lookupField(index) else typ)
            this match {
                case t: TRealClassObject =>
                    new TRealClassObject(t.cl, newFields, globalType)
                case _ =>
                    new TRealObject(newFields, globalType)
            }
        }

        // Used for type constructions
        def setAnyField(typ: Type) = {
            this match {
                case t: TRealClassObject =>
                    new TRealClassObject(t.cl, fields, typ)
                case _ =>
                    new TRealObject(fields, typ)
            }
        }

        def injectAnyField(typ: Type) = {
            var newFields = fields;
            // When the index is unknown, we have to pollute every entries
            for ((i,t) <- fields) {
                newFields = newFields.updated(i,t union typ)
            }

            this match {
                case t: TRealClassObject =>
                    new TRealClassObject(t.cl, newFields, globalType union typ)
                case _ =>
                    new TRealObject(newFields, globalType union typ)
            }
        }


        override def toString = {
            RecProtection.objectToStringDepth += 1;
            var r = "Object(?)"
            if (RecProtection.objectToStringDepth < 2) {
                r = r+"["+((fields.map(x => x._1 +" => "+ x._2).toList ::: "? -> "+globalType :: Nil).mkString("; "))+"]"
            } else {
                r = r+"[...]"
            }
            RecProtection.objectToStringDepth -= 1;
            r
        }

        def toText(e: TypeEnvironment) = toString

        def merge(a2: TRealObject): TRealObject = {
            // Pick superclass class, and subclass methods
            val newcl = (this, a2) match {
                case (o1: TRealClassObject, o2: TRealClassObject) =>
                    if (o1.cl.isSubtypeOf(o2.cl)) {
                        Some(o2.cl)
                    } else if (o2.cl.isSubtypeOf(o1.cl)) {
                        Some(o1.cl)
                    } else {
                        None
                    }
                case _ =>
                    None
            }

            var newFields = Map[String, Type]();

            for (index <- (fields.keySet ++ a2.fields.keySet)) {
                newFields = newFields.updated(index, lookupField(index) union a2.lookupField(index))
            }

            newcl match {
                case Some(cl) =>
                    new TRealClassObject(cl, newFields, globalType union a2.globalType)
                case None =>
                    new TRealObject(newFields, globalType union a2.globalType)
            }
        }
    }

    class TRealClassObject(val cl: TClass,
                           fields: Map[String, Type],
                           globalType: Type) extends TRealObject(fields, globalType) {

        override def toString = {
            RecProtection.objectToStringDepth += 1;
            var r = "Object("+cl+")"
            if (RecProtection.objectToStringDepth < 2) {
                r = r+"["+((fields.map(x => x._1 +" => "+ x._2).toList ::: "? -> "+globalType :: Nil).mkString("; "))+"]"
                r = r+"["+(cl.cs.methods.map(x => x._1+": "+x._2.ftyps.mkString("{", ",", "}")).mkString("; "))+"]"
            } else {
                r = r+"[...]"
            }
            RecProtection.objectToStringDepth -= 1;
            r
        }

        override def lookupMethod(index: String, from: Option[ClassSymbol]) =
            cl.cs.lookupMethod(index, from) match {
                case LookupResult(Some(ms), _, _) =>
                    // found method, ignore visibility errors, for now
                    // Type hints
                    if (ms.ftyps.size > 0) {
                        Some(ms.ftyps.toList.head)
                    } else {
                        None
                    }

                case LookupResult(None, _, _) =>
                    None
            }
    }

    class TArray(val entries: Map[String, Type], val globalType: Type) extends ConcreteType {

        def this() =
            this(Map[String, Type](), TUninitialized)

        def this(global: Type) =
            this(Map[String, Type](), global)

        def lookup(index: String): Type =
            entries.getOrElse(index, globalType)

        def lookupByType(typ: Type): Type = typ match {
            case TIntLit(v) => lookup(v+"")
            case TFloatLit(v) => lookup(v.toInt+"")
            case TStringLit(v) => lookup(v)
            case tu: TUnion =>
                (tu.types.map { lookupByType(_) }).reduceLeft(_ union _)
            case _ => globalType
        }

        def injectByType(indtyp: Type, typ: Type): TArray = indtyp match {
            case TIntLit(v) => inject(v+"", typ)
            case TFloatLit(v) => inject(v.toInt+"", typ)
            case TStringLit(v) => inject(v, typ)
            case tu: TUnion =>
                val weaktype = typ union globalType
                tu.types.foldLeft(this){ _.injectByType(_, weaktype) }
            case _ => injectAny(typ)
        }

        override def depth(env: TypeEnvironment): Int =
            globalType.depth(env).max(entries.map(_._2.depth(env)).foldLeft(0)(_ max _))+1

        def inject(index: String, typ: Type): TArray = {
            new TArray(entries + (index -> typ), globalType)
        }

        // used for type constructions
        def setAny(typ: Type): TArray = {
            new TArray(entries, typ)
        }

        def injectAny(typ: Type): TArray = {
            // When the index is unknown, we have to pollute every entries
            var newEntries = Map[String, Type]();
            for ((i,t) <- entries) {
                newEntries = newEntries + (i -> (t union typ))
            }

            new TArray(newEntries, globalType union typ)
        }

        def merge(a2: TArray): TArray = {
            var newEntries = Map[String, Type]()

            for (k <- a2.entries.keySet ++ entries.keySet) {
                newEntries = newEntries + (k -> (lookup(k) union a2.lookup(k)))
            }

            new TArray(newEntries, globalType union a2.globalType)
        }

        override def equals(t: Any): Boolean = t match {
            case ta: TArray =>
                entries == ta.entries && globalType == ta.globalType
            case _ => false
        }

        override def hashCode = {
            (entries.values.foldLeft(0)((a,b) => a ^ b.hashCode)) + globalType.hashCode
        }

        override def toText(env: TypeEnvironment) =
            "Array["+(entries.toList.sortWith((x,y) => x._1 < y._1).map(x => x._1 +" => "+ x._2.toText(env)).toList ::: "? => "+globalType.toText(env) :: Nil).mkString(", ")+"]"
        override def toString =
            "Array["+(entries.toList.sortWith((x,y) => x._1 < y._1).map(x => x._1 +" => "+ x._2).toList ::: "? => "+globalType :: Nil).mkString("; ")+"]"
    }

    object TAnyArray extends TArray(Map[String, Type](), TTop) {
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

    trait Typed {
        self =>

        private var _tpe: Type = TAny

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }


    def typeHintToType(oth: Option[AST.TypeHint]): Type = oth match {
        case Some(a) => typeHintToType(a)
        case None => TAny;
    }

    def typeHintToType(th: AST.TypeHint): Type = th match {
        case AST.THArray => TAnyArray
        case AST.THObject(AST.StaticClassRef(_, _, id)) =>
            GlobalSymbols.lookupClass(id.value) match {
                case Some(cs) =>
    //                ObjectStore.getOrCreateTMP(Some(cs))
                    TAnyObject
                case None =>
                    TAnyObject
            }
    }
}
