package phpanalysis.controlflow;
import analyzer.Symbols._
import scala.collection.mutable.HashSet


object Types {
    sealed abstract class Type {
        self=>

        def union(t: Type) = TUnion(this, t)

        // by default, contains only itself
        def contains(t: Type) = t match {
            case t: self.type =>
                true
            case _ =>
            false
        }

        def equals(t: Type) = t == self;

        def toText = toString
    }

    sealed abstract class ClassType
    case object TClassAny extends ClassType
    case class TClass(cd: ClassSymbol) extends ClassType
    case class TClassUnion(l: ClassType, r: ClassType) extends ClassType


    case object TInt extends Type
    case object TBoolean extends Type
    case object TFloat extends Type
    case object TString extends Type

    case object TResource extends Type
    case object TNull extends Type

    class TUnion extends Type {
        private val types = new HashSet[Type]();

        def add(t: Type) = t match {
            case t1: TUnion =>
                for (t2 <- t1.types) {
                    types += t2
                }
            case _ =>
                types += t
        }

        override def equals(t: Type) = t match {
            case tu: TUnion =>
                types  == tu.types
            case _ => false
        }

        override def toString = types.mkString("{", ",", "}")
        override def toText   = types.mkString(" or ")
    }

    object TUnion extends Type {
        def apply(t1: TUnion, t2: Type) = t1 add t2
        def apply(t1: Type, t2: TUnion) = t2 add t1
        def apply(t1: Type, t2: Type) = {
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

    case class TObject(cl: ClassType) extends Type

    case object TAnyObject extends Type

    case object TAny extends Type {
        override def contains(v: Type) = true;
    }

    trait Typed {
        self =>

        private var _tpe: Type = TAny

        def setType(tpe: Type): self.type = { _tpe = tpe; this }
        def getType: Type = _tpe
    }

}
