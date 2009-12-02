package phpanalysis.controlflow;
import analyzer.Symbols._
import scala.collection.mutable.{HashSet, HashMap, Map}

import TypeFlow._


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
    case class TClass(cd: ClassSymbol) extends ClassType {
        override def toString = cd.name
        def isSubtypeOf(cl2: ClassType) = cl2 match {
            case TClassAny => false
            case TClass(cd2) => cd.subclassOf(cd2)
        }
    }

    sealed abstract class MethodType
    case object TMethodAny extends MethodType
    case class TMethod(args: List[Type], ret: Type) extends MethodType {
        override def toString = args.mkString("(", ", ", ")")+" => "+ret
    }

    abstract class TObject extends Type {
        self =>
        def lookupField(index: String): Option[Type];
        def lookupMethod(index: String): Option[MethodType];
        def injectField(index: String, typ: Type) : self.type;
    }

    object TAnyObject extends TObject {
        def lookupField(index: String) =
            Some(TAny)
        def lookupMethod(index: String) =
            Some(TMethodAny);
        def injectField(index: String, typ: Type) =
            this

        override def toText = {
            "any object"
        }
    }

    class TPreciseObject(val cl: TClass,
                         val fields: Map[String, Type],
                         var pollutedType: Option[Type],
                         val methods: Map[String, TMethod]) extends TObject {

        def lookupField(index: String) =
            fields.get(index) match {
                case Some(t) => Some(t)
                case None => pollutedType
            }

        def lookupMethod(index: String) =
            methods.get(index)
        def injectField(index: String, typ: Type) =
            this

        override def toString = {
            var r = "Object("+cl+")"
            r = r+"["+(fields.map(x => x._1 +" => "+ x._2).mkString("; "))+(if(pollutedType != None) " ("+pollutedType.get+")" else "")+"]"
            r = r+"["+(methods.map(x => x._1 +" => "+ x._2).mkString("; "))+"]"
            r
        }

        def merge(a2: TPreciseObject): TObject = {
            // Pick superclass class, and subclass methods
            val newVals = if (cl.isSubtypeOf(a2.cl)) {
                (a2.cl, methods)
            } else if (a2.cl.isSubtypeOf(cl)) {
                (cl, a2.methods)
            } else {
                // Shortcut, incompatible objects
                return TAnyObject
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

            new TPreciseObject(newVals._1, newFields, newPollutedType, newVals._2)
        }
    }

    abstract class TArray extends Type {
        self=>
        import CFGTrees._
        def lookup(index: String): Option[Type];
        def lookup(index: CFGSimpleValue): Option[Type] = index match {
          case CFGNumLit(i)        => lookup(i+"")
          case CFGStringLit(index) => lookup(index)
          case _ => Some(TAny) // Should never happen without error in the parent
        }
        def inject(index: String, typ: Type): self.type;
        def inject(index: CFGSimpleValue, typ: Type): self.type = index match {
          case CFGNumLit(i)        => inject(i+"", typ)
          case CFGStringLit(index) => inject(index, typ)
          case _ => pollute(typ)
        }
        def injectNext(typ: Type, p: Positional): self.type;
        def pollute(typ: Type): self.type;
        def duplicate: TArray;
    }

    case object TAnyArray extends TArray {
        def lookup(index: String) = Some(TAny)
        def inject(index: String, typ: Type) = this
        def injectNext(typ: Type, p: Positional) = this
        def pollute(typ: Type) = this
        def duplicate = this
        override def toString = "Array[?]"
        override def toText = "any array"
    }

    class TPreciseArray(val entries: Map[String, Type],
                        var pollutedType: Option[Type],
                        var nextFreeIndex: Int) extends TArray {
        self =>

        var pushPositions = HashSet[String]()

        def this() = this(HashMap[String, Type](), None, 0)


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

        def merge(a2: TPreciseArray): TPreciseArray = {
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

            new TPreciseArray(newEntries, newPollutedType, max(nextFreeIndex, a2.nextFreeIndex))
        }

        override def equals(t: Any): Boolean = t match {
            case ta: TPreciseArray =>
                entries == ta.entries && pollutedType == ta.pollutedType
            case _ => false
        }

        def duplicate = {
            new TPreciseArray(HashMap[String, Type]() ++ entries, pollutedType, nextFreeIndex)
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
                    if (!(types contains t)) {
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
        def apply(t1: TUnion, t2: Type): Unit = t1 add t2
        def apply(t1: Type, t2: TUnion): Unit = t2 add t1
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
