package phpanalysis.analyzer;
import Symbols._
import scala.collection.mutable.{HashSet, HashMap, Map}

import controlflow.TypeFlow._


object Types {
    sealed abstract class Type {
        self=>

        def union(t: Type) = TUnion(this, t)

        def equals(t: Type) = t == self;

        def toText = toString
    }

    sealed abstract class ClassType {
        def isSubtypeOf(cl2: ClassType): Boolean;
    }
    case object TClassAny extends ClassType {
        def isSubtypeOf(cl2: ClassType) = true;
    }
    case class TClass(cs: ClassSymbol) extends ClassType {
        override def toString = cs.name
        def isSubtypeOf(cl2: ClassType) = cl2 match {
            case TClassAny => false
            case TClass(cs2) => cs.subclassOf(cs2)
        }
    }

    sealed abstract class FunctionType {
        val ret: Type;
    }
    case object TFunctionAny extends FunctionType {
        val ret = TAny
    }
    case class TFunction(args: List[(Type, Boolean)], ret: Type) extends FunctionType {

        override def toString = args.map{a => a match {
                case (t, false) => t
                case (t, true) => "["+t+"]"
            }}.mkString("(", ", ", ")")+" => "+ret
    }


    abstract class RealObjectType {
        self =>
        var fields: Map[String, Type]
        var pollutedType: Option[Type]
        def lookupField(index: String): Option[Type];
        def lookupMethod(index: String, from: Option[ClassSymbol]): Option[FunctionType];
        def injectField(index: String, typ: Type) : self.type;

        def polluteFields(typ: Type): self.type;
    }

    // Objects related types
    type ObjectId = Int

    // Stores the ref => Real Objects relashionship
    object ObjectStore {
        private val store = new HashMap[ObjectId, RealObjectType]();
        def lookup(id: ObjectId): RealObjectType = store.get(id) match {
            case Some(o) => o
            case None => error("Woops incoherent store")
        }

        def set(id: ObjectId, robj: RealObjectType) = store(id) = robj;

        def getOrCreate(id: ObjectId, ocs: Option[ClassSymbol]) : TObjectRef = store.get(id) match {
            case Some(_) =>
                TObjectRef(id)
            case None =>
                // We create a new object and place it in the store
                val rot = ocs match {
                    case Some(cs) =>
                        // construct a default object for this class
                        TRealClassObject(TClass(cs), HashMap[String,Type]() ++ cs.properties.mapElements[Type] { x => x.typ }, None)
                    case None =>
                        // No class => any object
                        TRealObject(HashMap[String,Type](), None)
                }

                store(id) = rot;
                TObjectRef(id)
        }
    }

    // Object types exposed to symbols
    abstract class ObjectType extends Type {
        self=>
        import controlflow.CFGTrees._
        def pollutedType: Option[Type];

        def lookupField(index: String): Option[Type];
        def lookupField(index: CFGSimpleValue): Option[Type] = index match {
          case CFGNumLit(i)        => lookupField(i+"")
          case CFGStringLit(index) => lookupField(index)
          case _ => pollutedType
        }

        def injectField(index: String, typ: Type): self.type;
        def injectField(index: CFGSimpleValue, typ: Type): self.type = index match {
          case CFGNumLit(i)        => injectField(i+"", typ)
          case CFGStringLit(index) => injectField(index, typ)
          case _ => polluteFields(typ)
        }

        def lookupMethod(index: String, from: Option[ClassSymbol]): Option[FunctionType];
        def lookupMethod(index: CFGSimpleValue, from: Option[ClassSymbol]): Option[FunctionType] = index match {
          case CFGNumLit(i)        => lookupMethod(i+"", from)
          case CFGStringLit(index) => lookupMethod(index, from)
          case _ => Some(TFunctionAny)
        }

        def polluteFields(typ: Type): self.type;

    }
    // Any object, should be only used to typecheck, no symbol should be infered to this type
    case object TAnyObject extends ObjectType {
        def lookupField(index: String) = pollutedType
        def injectField(index: String, typ: Type) = this

        def lookupMethod(index: String, from: Option[ClassSymbol]) = Some(TFunctionAny)

        def pollutedType = Some(TAny)
        def polluteFields(typ: Type) = this
    }
    // Reference to an object in the store
    case class TObjectRef(val id: ObjectId) extends ObjectType {
        def realObj = ObjectStore.lookup(id)

        def lookupField(index: String) = {
            realObj.lookupField(index)
        }

        def injectField(index: String, typ: Type) = {
            realObj.injectField(index, typ)
            this
        }

        def lookupMethod(index: String, from: Option[ClassSymbol]) = {
            realObj.lookupMethod(index, from)
        }

        def polluteFields(typ: Type) = {
            realObj.polluteFields(typ)
            this
        }

        def pollutedType = realObj.pollutedType

        override def toString = {
            "(#"+id+"->"+realObj+")"
        }
    }

    // Real object type (in the store) representing a specific object of any class
    case class TRealObject(var fields: Map[String, Type],
                           var pollutedType: Option[Type]) extends RealObjectType {

        def lookupField(index: String) =
            fields.get(index) match {
                case Some(t) => Some(t)
                case None => pollutedType
            }

        def lookupMethod(index: String, from: Option[ClassSymbol]): Option[FunctionType] =
            None

        def injectField(index: String, typ: Type) = {
            fields(index) = typ;
            this
        }

        override def toString = {
            var r = "Object(?)"
            r = r+"["+(fields.map(x => x._1 +" => "+ x._2).mkString("; "))+(if(pollutedType != None) " ("+pollutedType.get+")" else "")+"]"
            r
        }

        def polluteFields(typ: Type) = {

            // When the index is unknown, we have to pollute every entries
            for ((i,t) <- fields) {
                fields(i) = t union typ
            }
            // we flag the array to allow lookup to return Any instead of None
            // since the key=>value relationship is not longer safe
            pollutedType = pollutedType match { 
                case Some(pt) => Some(pt union typ)
                case None => Some(typ)
            }

            this
        }
        def merge(a2: TRealObject): RealObjectType = {
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
                case (o1: TRealClassObject, o2 : TRealObject) =>
                    Some(o1.cl)
                case (o1: TRealObject, o2 : TRealClassObject) =>
                    Some(o2.cl)
                case _ =>
                    None
            }

            val newPollutedType = (pollutedType, a2.pollutedType) match {
                case (Some(pt1), Some(pt2)) => Some(TypeLattice.join(pt1, pt2))
                case (Some(pt1), None) => Some(pt1)
                case (None, Some(pt2)) => Some(pt2)
                case (None, None) => None
            }

            val newFields = HashMap[String, Type]() ++ fields;

            for((index, typ)<- a2.fields) {
                newFields(index) = newFields.get(index) match {
                    case Some(t) => TypeLattice.join(t, typ)
                    case None => typ
                }
            }

            newcl match {
                case Some(cl) =>
                    new TRealClassObject(cl, newFields, newPollutedType)
                case None =>
                    new TRealObject(newFields, newPollutedType)
            }
        }
    }

    case class TRealClassObject(val cl: TClass,
                                initFields: Map[String, Type],
                                initPollutedType: Option[Type]) extends TRealObject(initFields, initPollutedType){

        override def toString = {
            var r = "Object("+cl+")"
            r = r+"["+(fields.map(x => x._1 +" => "+ x._2).mkString("; "))+(if(pollutedType != None) " ("+pollutedType.get+")" else "")+"]"
            r = r+"["+(cl.cs.methods.map(x => x._1+": "+msToTMethod(x._2)).mkString("; "))+"]"
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

    abstract class ArrayType(pollutedType: Option[Type]) extends Type {
        self=>
        import controlflow.CFGTrees._
        def lookup(index: String): Option[Type];
        def lookup(index: CFGSimpleValue): Option[Type] = index match {
          case CFGNumLit(i)        => lookup(i+"")
          case CFGStringLit(index) => lookup(index)
          case _ => pollutedType
        }
        def inject(index: String, typ: Type): self.type;
        def inject(index: CFGSimpleValue, typ: Type): self.type = index match {
          case CFGNumLit(i)        => inject(i+"", typ)
          case CFGStringLit(index) => inject(index, typ)
          case _ => pollute(typ)
        }
        def injectNext(typ: Type, p: Positional): self.type;
        def pollute(typ: Type): self.type;
        def duplicate: ArrayType;
    }

    case object TAnyArray extends ArrayType(Some(TAny)) {
        def lookup(index: String) = Some(TAny)
        def inject(index: String, typ: Type) = this
        def injectNext(typ: Type, p: Positional) = this
        def pollute(typ: Type) = this
        def duplicate = this
        override def toString = "Array[?]"
        override def toText = "any array"
    }

    class TArray(val entries: Map[String, Type],
                 var pollutedType: Option[Type],
                 var nextFreeIndex: Int) extends ArrayType(pollutedType) {
        self =>

        var pushPositions = HashSet[String]()

        def this() = this(HashMap[String, Type](), None, 0)
        def this(pollutedType: Type) = this(HashMap[String, Type](), Some(pollutedType), 0)


        def inject(index: String, typ: Type) = {
            // Used to inject and specific entry=>type relationship
            entries += ((index, typ))
            this
        }

        def injectNext(typ: Type, p: Positional) = {
            if (!(pushPositions contains p.getPos)) {
                // Used to inject and specific entry=>type relationship
                while(entries.get(nextFreeIndex+"") != None) {
                    nextFreeIndex += 1;
                }
                entries += ((nextFreeIndex+"", typ))
                pushPositions += p.getPos
            }
            this
        }

        def pollute(typ: Type) = {
            // When the index is unknown, we have to pollute every entries
            for ((i,t) <- entries) {
                entries(i) = t union typ
            }
            // we flag the array to allow lookup to return Any instead of None
            // since the key=>value relationship is not longer safe
            pollutedType = pollutedType match { 
                case Some(pt) => Some(pt union typ)
                case None => Some(typ)
            }

            this
        }

        def lookup(index: String): Option[Type] = {
            entries.get(index) match {
                case Some(t) => Some(t)
                case None =>
                    // If the array has been polluted, we return the best guess
                    pollutedType
            }
        }

        def merge(a2: TArray): TArray = {
            import Math.max

            val newPollutedType = (pollutedType, a2.pollutedType) match {
                case (Some(pt1), Some(pt2)) => Some(TypeLattice.join(pt1, pt2))
                case (Some(pt1), None) => Some(pt1)
                case (None, Some(pt2)) => Some(pt2)
                case (None, None) => None
            }

            val newEntries = HashMap[String, Type]() ++ entries;

            for((index, typ)<- a2.entries) {
                newEntries(index) = newEntries.get(index) match {
                    case Some(t) => TypeLattice.join(t, typ)
                    case None => typ
                }
            }

            new TArray(newEntries, newPollutedType, max(nextFreeIndex, a2.nextFreeIndex))
        }

        override def equals(t: Any): Boolean = t match {
            case ta: TArray =>
                entries == ta.entries && pollutedType == ta.pollutedType
            case _ => false
        }

        def duplicate = {
            new TArray(HashMap[String, Type]() ++ entries, pollutedType, nextFreeIndex)
        }

        override def toString =
            "Array["+(entries.map(x => x._1 +" => "+ x._2).mkString("; "))+(if(pollutedType != None) " ("+pollutedType.get+")" else "")+"]"
    }


    case object TInt extends Type {
        override def toText = "int"
    }
    case object TBoolean extends Type {
        override def toText = "boolean"
    }
    case object TTrue extends Type {
        override def toText = "true"
    }
    case object TFalse extends Type {
        override def toText = "false"
    }

    case object TFloat extends Type {
        override def toText = "float"
    }
    case object TString extends Type {
        override def toText = "string"
    }
    case object TAny extends Type {
        override def toText = "any"
    }
    case object TNone extends Type {
        override def toText = "none"
    }
    case object TResource extends Type {
        override def toText = "resource"
    }
    case object TNull extends Type {
        override def toText = "null"
    }

    class TUnion extends Type {
        var types: List[Type] = Nil

        def add(t: Type) = t match {
            case t1: TUnion =>
                for (t2 <- t1.types) {
                    if (!(types contains t2)) {
                        types = t2 :: types
                    }
                }
            case _ =>
                if (!(types contains t)) {
                    types = t :: types
                }
        }

        override def equals(t: Any) = t match {
            case tu: TUnion =>
                (new HashSet[Type]() ++ types)  == (new HashSet[Type]() ++ tu.types)
            case _ => false
        }

        override def toString = types.mkString("{", ",", "}")
        override def toText   = types.map { x => x.toText }.mkString(" or ")
    }

    object TUnion extends Type {
        def apply(t1: TUnion, t2: Type): Type = {
            t1 add t2
            t1
        }
        def apply(t1: Type, t2: TUnion): Type = {
            t2 add t1
            t2
        }
        def apply(ts: List[Type]): Type = {
            if (ts.size == 1) {
                ts.head
            } else if (ts.size > 1) {
                val t = new TUnion;
                t.types = ts;
                t
            } else {
                TNone
            }
        }
        def apply(t1: Type, t2: Type): Type = {
            if (t1 == t2) {
                t1
            } else {
                val t = new TUnion;
                t add t1
                t add t2
                t
            }
        }

    }


    trait Typed {
        self =>

        private var _tpe: Type = TAny

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }

}
