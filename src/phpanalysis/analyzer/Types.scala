package phpanalysis.analyzer;
import Symbols._
import scala.collection.immutable.{Map, Set}

import controlflow.TypeFlow._
import controlflow.CFGTrees._
import parser.Trees._

object Types {
    object RecProtection {
        var objectToStringDeep = 0;
    }

    sealed abstract class Type {
        self=>

        def union(t: Type) = TypeLattice.join(this, t)
        def join(t: Type)  = union(t)

        def equals(t: Type) = t == self;

        def toText(te: TypeEnvironment) = toString
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
    class TFunction(val args: List[(Type, Boolean)], val ret: Type) extends FunctionType {

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
        def set(id: ObjectId, robj: TRealObject): ObjectStore = new ObjectStore(store.update(id, robj));

        def initIfNotExist(id: ObjectId, ocs: Option[ClassSymbol]) : ObjectStore = store.get(id) match {
            case Some(_) =>
                this
            case None =>
                // We create a new object and place it in the store
                val rot = ocs match {
                    case Some(cs) =>
                        // construct a default object for this class
                        new TRealClassObject(new TClass(cs), Map[String,Type]() ++ cs.properties.mapElements[Type] { x => x.typ }, TUninitialized)
                    case None =>
                        // No class => any object
                        new TRealObject(Map[String,Type](), TUninitialized)
                }

                set(id, rot);
        }

        override def toString = {
            store.toList.sort{(x,y) => x._1.pos < x._1.pos}.map(x => "("+x._1.pos+","+x._1.offset+") => "+x._2).mkString("{ ", "; ", " }");
        }
    }

    // Object types exposed to symbols
    abstract class ObjectType extends ConcreteType

    // Any object, should be only used to typecheck, no symbol should be infered to this type
    object TAnyObject extends ObjectType {
        override def toString = "TAnyObject"
        override def toText(te: TypeEnvironment)   = "any object"
    }
    // Reference to an object in the store
    class TObjectRef(val id: ObjectId) extends ObjectType {
        override def toString = {
            "TObjectRef#"+id+""
        }

        override def toText(te: TypeEnvironment) = {
            te.store.lookup(id).toText(te)
        }

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

        def lookupField(index: CFGSimpleValue): Type = index match {
          case CFGLong(i)        => lookupField(i+"")
          case CFGString(index) => lookupField(index)
          case _ => globalType
        }

        def lookupField(index: String) =
            fields.getOrElse(index, globalType)

        def lookupMethod(index: String, from: Option[ClassSymbol]): Option[FunctionType] = None

        def lookupMethod(index: CFGSimpleValue, from: Option[ClassSymbol]): Option[FunctionType] = index match {
            case CFGLong(i)        => lookupMethod(i+"", from)
            case CFGString(index) => lookupMethod(index, from)
            case _ => None
        }

        def injectField(index: CFGSimpleValue, typ: Type): TRealObject =
            injectField(index, typ, true)

        def injectField(index: CFGSimpleValue, typ: Type, weak: Boolean): TRealObject = index match {
          case CFGLong(i)       => injectField(i+"",  typ, weak)
          case CFGString(index) => injectField(index, typ, weak)
          case _ => injectAnyField(typ)
        }

        def injectField(index: String, typ: Type): TRealObject =
            injectField(index, typ, true)


        def injectField(index: String, typ: Type, weak: Boolean): TRealObject = {
            val newFields = fields.update(index, if (weak) typ union lookupField(index) else typ)
            this match {
                case t: TRealClassObject =>
                    new TRealClassObject(t.cl, newFields, globalType)
                case _ =>
                    new TRealObject(newFields, globalType)
            }
        }

        def injectAnyField(typ: Type) = {
            var newFields = fields;
            // When the index is unknown, we have to pollute every entries
            for ((i,t) <- fields) {
                newFields = newFields.update(i,t union typ)
            }

            this match {
                case t: TRealClassObject =>
                    new TRealClassObject(t.cl, newFields, globalType union typ)
                case _ =>
                    new TRealObject(newFields, globalType union typ)
            }
        }


        override def toString = {
            RecProtection.objectToStringDeep += 1;
            var r = "Object(?)"
            if (RecProtection.objectToStringDeep < 2) {
                r = r+"["+((fields.map(x => x._1 +" => "+ x._2).toList ::: "? -> "+globalType :: Nil).mkString("; "))+"]"
            } else {
                r = r+"[...]"
            }
            RecProtection.objectToStringDeep -= 1;
            r
        }

        def toText(te: TypeEnvironment) = toString

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
                newFields = newFields.update(index, lookupField(index) union a2.lookupField(index))
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
            RecProtection.objectToStringDeep += 1;
            var r = "Object("+cl+")"
            if (RecProtection.objectToStringDeep < 2) {
                r = r+"["+((fields.map(x => x._1 +" => "+ x._2).toList ::: "? -> "+globalType :: Nil).mkString("; "))+"]"
                r = r+"["+(cl.cs.methods.map(x => x._1+": "+msToTMethod(x._2)).mkString("; "))+"]"
            } else {
                r = r+"[...]"
            }
            RecProtection.objectToStringDeep -= 1;
            r
        }

        def msToTMethod(ms: MethodSymbol) = {
            new TFunction(ms.argList.map{ x => (x._2.typ, x._2.optional)}.toList, ms.typ)
        }

        override def lookupMethod(index: String, from: Option[ClassSymbol]) =
            cl.cs.lookupMethod(index, from) match {
                case LookupResult(Some(ms), _, _) =>
                    // found method, ignore visibility errors, for now
                    // Type hints
                    Some(msToTMethod(ms))

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

        def lookup(index: CFGSimpleValue): Type = index match {
          case CFGLong(i)       => lookup(i+"")
          case CFGString(index) => lookup(index)
          case CFGConstant(id) =>
            GlobalSymbols.lookupConstant(id.value) match {
                case None =>
                    // PHP falls back to the constant name as a string
                    lookup(id.value)
                case Some(cs) =>
                    cs.value match {
                        case Some(v) =>
                            lookup(Evaluator.scalarToString(v))
                        case None =>
                            globalType
                    }
            }

          case _ => globalType
        }

        def inject(index: String, typ: Type): TArray =
            new TArray(entries + (index -> typ), globalType)

        def inject(index: CFGSimpleValue, typ: Type): TArray = index match {
          case CFGLong(i)       => inject(i+"", typ)
          case CFGString(index) => inject(index, typ)
          case CFGConstant(id) =>
            GlobalSymbols.lookupConstant(id.value) match {
                case None =>
                    // PHP falls back to the constant name as a string
                    inject(id.value, typ)
                case Some(cs) =>
                    cs.value match {
                        case Some(v) =>
                            inject(Evaluator.scalarToString(v), typ)
                        case None =>
                            injectAny(typ)
                    }
            }
          case _ => injectAny(typ)
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
            var newEntries = Map[String, Type]() ++ entries;

            for((index, typ)<- a2.entries) {
                newEntries = newEntries.update(index, newEntries.get(index) match {
                    case Some(t) => TypeLattice.join(t, typ)
                    case None => TypeLattice.join(typ, TUninitialized)
                })
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

        override def toString =
            "Array["+(entries.map(x => x._1 +" => "+ x._2).toList ::: "? => "+globalType :: Nil).mkString("; ")+"]"
    }

    object TAnyArray extends TArray(Map[String, Type](), TTop) {
        override def toString = "Array[?]"
        override def toText(te: TypeEnvironment) = "any array"

        override def equals(t: Any): Boolean = t match {
            case r: AnyRef =>
                this eq r
            case _ => false
        }
    }

    case object TInt extends ConcreteType {
        override def toText(te: TypeEnvironment) = "int"
    }
    case object TBoolean extends ConcreteType {
        override def toText(te: TypeEnvironment) = "boolean"
    }
    case object TTrue extends ConcreteType {
        override def toText(te: TypeEnvironment) = "true"
    }
    case object TFalse extends ConcreteType {
        override def toText(te: TypeEnvironment) = "false"
    }

    case object TFloat extends ConcreteType {
        override def toText(te: TypeEnvironment) = "float"
    }
    case object TString extends ConcreteType {
        override def toText(te: TypeEnvironment) = "string"
    }
    case object TAny extends ConcreteType {
        override def toText(te: TypeEnvironment) = "any"
    }
    case object TResource extends ConcreteType {
        override def toText(te: TypeEnvironment) = "resource"
    }
    case object TNull extends ConcreteType {
        override def toText(te: TypeEnvironment) = "null"
    }

    /* Special types */
    case object TTop extends Type {
        override def toText(te: TypeEnvironment) = "top"
    }

    case object TBottom extends Type {
        override def toText(te: TypeEnvironment) = "bottom"
    }

    case object TUninitialized extends Type {
        override def toText(te: TypeEnvironment) = "uninitialized"
    }

    object TUnion {
        def apply(ts: Iterable[Type]) = {

            var tlist: List[Type] = Nil

            for (t <- ts) {
                tlist = addToList(tlist, t)
            }

            if (tlist.size == 0) {
                TBottom
            } else if (tlist.size == 1) {
                tlist.head
            } else {
                new TUnion(tlist)
            }
        }

        def apply(t1: Type, t2: Type) = {
            if (t1 == t2) {
                t1
            } else {
                new TUnion(t1, t2)
            }
        }

        def getList(t1: Type, t2: Type) = (t1, t2) match {
            case (_, tu: TUnion) =>
                addToList(tu.types, t1)
            case (tu: TUnion, _) =>
                addToList(tu.types, t2)
            case (_, _) =>
                addToList(List(t1), t2)
        }

        def addToList(typs: List[Type], typ: Type): List[Type] = {
            val res = typ match {
                case tu: TUnion =>
                    var res: List[Type] = typs
                    for (t <- tu.types) {
                        res = addToList(res, t)
                    }
                    res
                case TBoolean =>
                    TBoolean :: typs.filter{t => t != TFalse && t != TTrue && t != TBoolean}.toList

                case TFalse =>
                    if ((typs contains TFalse) || (typs contains TBoolean)) {
                        typs
                    } else {
                        if (typs contains TTrue) {
                            TBoolean :: typs.filter{t => t != TTrue}.toList
                        } else {
                            TFalse :: typs
                        }
                    }
                case TTrue =>
                    if ((typs contains TTrue) || (typs contains TBoolean)) {
                        typs
                    } else {
                        if (typs contains TFalse) {
                            TBoolean :: typs.filter{t => t != TFalse}.toList
                        } else {
                            TTrue :: typs
                        }
                    }

                case TAnyArray =>
                    TAnyArray :: typs.filter{! _.isInstanceOf[TArray]}.toList

                case ta: TArray =>
                    if (typs contains TAnyArray) {
                        typs
                    } else {
                        var nt: List[Type] = Nil
                        var toAdd: Type = ta

                        for (tc <- typs) tc match {
                            case t: TArray =>
                                toAdd = toAdd union tc
                            case _ =>
                                nt = tc :: nt
                        }
                        toAdd :: nt;
                    }
                case _ =>
                    if (!(typs contains typ)) {
                        typ :: typs
                    } else {
                        typs
                    }
            }

            for (t <- res) t match {
                case _: TUnion =>
                    println("WOOOOOOOOOOT: addToList("+typs+", "+typ+") includes TUnion!")
                case _ =>
            }

            res
        }
    }

    class TUnion(val types: List[Type]) extends Type {

        def this(t1: Type, t2: Type) =
            this(TUnion.getList(t1, t2))

        override def equals(t: Any): Boolean = t match {
            case tu: TUnion =>
                if (tu.types.size == types.size) {
                    for (v <- types) {
                        if (!(tu.types contains v)) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            case _ => false
        }

        override def toString = types.mkString("{", ",", "}")
        override def toText(te: TypeEnvironment)   = types.map { x => x.toText(te) }.mkString(" or ")

        if (types.size < 2) throw new RuntimeException("TUnion should at least be 2 types!")

        override def hashCode = {
            (types.foldLeft(0)((a,b) => a ^ b.hashCode))
        }
    }

    trait Typed {
        self =>

        private var _tpe: Type = TAny

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }


    def typeHintToType(oth: Option[TypeHint]): Type = oth match {
        case Some(a) => typeHintToType(a)
        case None => TAny;
    }

    def typeHintToType(th: TypeHint): Type = th match {
        case THString => TString
        case THAny => TAny
        case THFalse => TFalse
        case THTrue => TTrue
        case THResource => TResource
        case THInt => TInt
        case THBoolean => TBoolean
        case THFloat => TFloat
        case THNull => TNull
        case THArray => TAnyArray
        case THAnyObject => TAnyObject
        case THObject(StaticClassRef(_, _, id)) =>
            GlobalSymbols.lookupClass(id.value) match {
                case Some(cs) =>
    //                ObjectStore.getOrCreateTMP(Some(cs))
                    TAnyObject
                case None =>
                    println("Woops, undefined class "+id.value)
                    TAnyObject
            }
        case u: THUnion =>
            typeHintToType(u.a) union typeHintToType(u.b)
    }
}
