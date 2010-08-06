package phantm.types

import phantm.lattice.Lattice

case object TypeLattice extends Lattice {
    type Env = TypeEnvironment
    type E = Type

    def leq(x : Type, y : Type): Boolean = leq(BaseTypeEnvironment, x, y)

    def leq(env: Env, x : Type, y : Type): Boolean = leq(env, env, x, y)

    def leq(envx: Env, envy: Env, x : Type, y : Type): Boolean = {
        def leqT(x: Type, y: Type): Boolean = (x,y) match {
            case (x, y) if x == y => true
            case (TBottom, _) => true
            case (_, TTop) => true
            case (_:ConcreteType, TAny) => true
            case (_:TStringLit, TString) => true
            case (_:TIntLit, TInt) => true
            case (_:TFloatLit, TFloat) => true
            case (_:TNumericLit, TNumeric) => true
            case (TInt, TNumeric) => true
            case (TFloat, TNumeric) => true
            case (TTrue, TBoolean) => true
            case (TFalse, TBoolean) => true
            case (t1: TPreciseObject, TAnyObject) => true
            case (t1: TObjectRef, t2: TObjectRef) if t1.id == t2.id => true
            case (t1: TPreciseObject, t2: TPreciseObject)  =>
                val r1 = t1.realObject(envx)
                val r2 = t2.realObject(envy)

                val classesMatch = r1.ct isSubtypeOf r2.ct

                classesMatch && leqT(r1.globalType, r2.globalType) && ((r1.fields.keySet ++ r2.fields.keySet) forall (k =>
                    leqT(r1.lookupField(k), r1.lookupField(k))))

            case (t1: TArray, t2: TArray) =>
                leqT(t1.globalInt, t2.globalInt) && leqT(t1.globalString, t2.globalString) && 
                  ((t1.entries.keySet ++ t2.entries.keySet) forall (k =>
                    leqT(t1.lookup(k), t2.lookup(k)))
                  )

            case (t1: TUnion, t2: TUnion) =>
                t1.types forall { x => t2.types.exists { y => leqT(x, y) } }
            case (t1, t2: TUnion) =>
                t2.types exists { x => leqT(t1, x) }
            case (t1: TUnion, t2) =>
                t1.types forall { x => leqT(x, t2) }
            case _ => false
        }

        leqT(x,y)
    }

    val top = TTop
    val bottom = TBottom

    def join(x: Type, y: Type): Type = join(BaseTypeEnvironment, x, y)._2
    def join(envInit: Env, x: Type, y: Type): (Env, Type) = {
        var env = envInit
        def joinTypes(x: Type, y: Type): Type = (x,y) match {
            case (TTop, _) => TTop
            case (_, TTop) => TTop

            case (TBottom, _) => y
            case (_, TBottom) => x

            case (TAny, TUninitialized) => TTop
            case (TUninitialized, TAny) => TTop

            case (TAny, _: ConcreteType) => TAny
            case (_: ConcreteType, TAny) => TAny

            case (TAny, tu: TUnion) =>
                if (!(tu.types contains TUninitialized)) {
                    TAny
                } else {
                    TTop
                }
            case (tu: TUnion, TAny) =>
                if (!(tu.types contains TUninitialized)) {
                    TAny
                } else {
                    TTop
                }

            case (TNumeric, _:TNumericLit) => TNumeric
            case (_:TNumericLit, TNumeric) => TNumeric

            case (_:TFloatLit, TFloat)  => TFloat
            case (TFloat, _:TFloatLit)  => TFloat
            case (_:TIntLit, TInt)      => TInt
            case (TInt, _:TIntLit)      => TInt

            case (TFloat,   TInt)       => TNumeric
            case (TInt,     TFloat)     => TNumeric
            case (TNumeric, TInt)       => TNumeric
            case (TNumeric, TFloat)     => TNumeric
            case (TFloat,   TNumeric)   => TNumeric
            case (TInt,     TNumeric)   => TNumeric

            case (TTrue,    TFalse)     => TBoolean
            case (TFalse,   TTrue)      => TBoolean
            case (TTrue,    TBoolean)   => TBoolean
            case (TBoolean, TTrue)      => TBoolean
            case (TFalse,   TBoolean)   => TBoolean
            case (TBoolean, TFalse)     => TBoolean

            case (t1, t2) if t1 == t2 => t1

            // Objects
            case (TAnyObject, t: TPreciseObject) => TAnyObject
            case (t: TPreciseObject, TAnyObject) => TAnyObject
            case (t1: TObjectRef, t2: TObjectRef) =>
                if (t1.id != t2.id) {
                    // Different ids -> union
                    TUnion(t1, t2)
                } else {
                    t1
                }
            case (t1: TPreciseObject, t2: TPreciseObject) =>
                println("TODO: join of two non-ref objects")
                TAnyObject

            // Arrays
            case (TAnyArray, t: TArray) => TAnyArray
            case (t: TArray, TAnyArray) => TAnyArray
            case (t1: TArray, t2: TArray) =>
                var newEntries = Map[ArrayKey, Type]();

                for (k <- t1.entries.keySet ++ t2.entries.keySet) {
                    newEntries = newEntries.updated(k, joinTypes(t1.lookup(k), t2.lookup(k)))
                }

                new TArray(newEntries, joinTypes(t1.globalInt, t2.globalInt), joinTypes(t1.globalString, t2.globalString))
            // Unions
            case (t1, t2) => TUnion(t1, t2)
        }

        (env, joinTypes(x,y))
    }

    def joinObjects(envInit: TypeEnvironment, a1: TRealObject,  a2: TRealObject): (TypeEnvironment, TRealObject) = {
        var env = envInit

        def joinTypes(t1: Type, t2: Type): Type = {
            val (nenv, t) = TypeLattice.join(env, t1, t2)
            env = nenv
            t
        }

        // Pick superclass
        val newct = if (a1.ct.isSubtypeOf(a2.ct)) {
                a2.ct
            } else if (a2.ct.isSubtypeOf(a1.ct)) {
                a1.ct
            } else {
                TAnyClass
            }

        var newFields = Map[String, Type]();

        for (index <- (a1.fields.keySet ++ a2.fields.keySet)) {
            newFields = newFields.updated(index, joinTypes(a1.lookupField(index), a2.lookupField(index)))
        }

        val t = new TRealObject(newFields, joinTypes(a1.globalType, a2.globalType), a1.singleton && a2.singleton, newct)

        (env, t)
    }

    def meetObjects(envInit: TypeEnvironment, a1: TRealObject, a2: TRealObject): (TypeEnvironment, TRealObject) = {
        var env = envInit

        def meetTypes(t1: Type, t2: Type): Type = {
            val (nenv, t) = TypeLattice.meet(env, t1, t2)
            env = nenv
            t
        }

        // Pick subclass
        val newct = if (a1.ct.isSubtypeOf(a2.ct)) {
                a1.ct
            } else if (a2.ct.isSubtypeOf(a1.ct)) {
                a2.ct
            } else {
                TAnyClass
            }

        var newFields = Map[String, Type]();

        for (index <- (a1.fields.keySet ++ a2.fields.keySet)) {
            newFields = newFields.updated(index, meetTypes(a1.lookupField(index), a2.lookupField(index)))
        }

        val t = new TRealObject(newFields, meetTypes(a1.globalType, a2.globalType), a1.singleton && a2.singleton, newct)

        (env, t)
    }


    // For meet we actually require the environment, since object types
    // will be updated in the store
    def meet(x : Type, y : Type): Type = meet(BaseTypeEnvironment, x, y)._2
    def meet(envInit: Env, x : Type, y : Type): (TypeEnvironment, Type) = {
        var env = envInit
        def joinTypes(x: Type, y: Type): Type = {
            val (newEnv, t) = join(env, x, y)
            env = newEnv
            t
        }
        def meetTypes(x: Type, y: Type): Type = (x,y) match {
            case (TTop, _) => y
            case (_, TTop) => x

            case (TBottom, _) => TBottom
            case (_, TBottom) => TBottom

            case (TAny, _: ConcreteType) => y
            case (_: ConcreteType, TAny) => x

            case (TAny, tu: TUnion) =>
                if (tu.types contains TUninitialized) {
                    tu.types.filter(_ != TUninitialized).foldLeft(TBottom: Type)((x, y) => joinTypes(x, y))
                } else {
                    tu
                }
            case (tu: TUnion, TAny) =>
                if (tu.types contains TUninitialized) {
                    tu.types.filter(_ != TUninitialized).foldLeft(TBottom: Type)((x, y) => joinTypes(x, y))
                } else {
                    tu
                }

            case (t1, t2) if t1 == t2 => t1

            // Arrays
            case (t1: TArray, t2: TArray) =>
                var newEntries = Map[ArrayKey, Type]();

                for (k <- t1.entries.keySet ++ t2.entries.keySet) {
                    newEntries = newEntries.updated(k, meetTypes(t1.lookup(k), t2.lookup(k)))
                }

                new TArray(newEntries, meetTypes(t1.globalInt, t2.globalInt), meetTypes(t1.globalString, t2.globalString))


            // Unions
            case (tu1: TUnion, tu2: TUnion) =>
                var resUnion = Set[Type]();

                // we take the intersection
                for (t1 <- tu1.types; t2 <- tu2.types) {
                   resUnion = resUnion + meetTypes(t1, t2);
                }

                resUnion.foldLeft(TBottom: Type)((x, y) => joinTypes(x, y))

            case (tu1: TUnion, t2) =>
                var resUnion = Set[Type]();

                // we take the intersection
                for (t1 <- tu1.types) {
                   resUnion = resUnion + meetTypes(t1, t2);
                }

                resUnion.foldLeft(TBottom: Type)((x, y) => joinTypes(x, y))

            case (t1, tu2: TUnion) =>
                meetTypes(tu2, t1)

            // Arbitrary types
            case (t1, t2) =>
                if (leq(env, t1, t2)) {
                    t1
                } else if (leq(env, t2, t1)) {
                    t2
                } else {
                    TBottom
                }
        }

        (env, meetTypes(x,y))
    }
}
