package phantm.ast

import Trees.Tree

sealed class DummyContext;

abstract class ASTSimpleTraversal(root: Tree) extends ASTTraversal[DummyContext](root, new DummyContext) {

    def visit(tr: Tree): Boolean;

    def traverse (visit: Tree => Boolean): Unit = {
        def visit0(t: Tree, ctx: DummyContext): (DummyContext, Boolean) = (ctx, visit(t))
        super.traverse(visit0)
    }

    def execute = traverse(visit _)

}

abstract class ASTTraversal[UserDataType](root: Tree, initCtx: UserDataType) {

    def elements(p: Product) = (0 until p.productArity).map(p.productElement(_))

    /* Reduces from different structures to Trees */
    def addRec(el: Any, ctx: UserDataType): List[(Tree, UserDataType)] = {
        el match {
            case Some(n) =>
                addRec(n, ctx)
            case None =>
                List()
            case node: Tree =>
                List((node, ctx))
            case (t1, t2) =>
                addRec(t1, ctx) ::: addRec(t2, ctx)
            case (t1, t2, t3) =>
                addRec(t1, ctx) ::: addRec(t2, ctx) ::: addRec(t3, ctx)
            case ns2: List[_] =>
                ns2 flatMap {n => addRec(n, ctx)}
            case _ =>
                List()
        }
    }

    def traverse (visit: (Tree, UserDataType) => (UserDataType, Boolean)): Unit = {
        var nodes: List[(Tree, UserDataType)] = (root, initCtx) :: Nil;

        def traverse0: Unit = nodes match {
            case (node, ctx) :: ns => {
                visit(node, ctx) match {
                    case (newCtx, continue) =>
                        nodes = ns

                        if (continue) {
                            /* DFS */
                            node match {
                                case p: Product =>
                                    nodes = (elements(p) flatMap { el: Any => addRec(el, newCtx) } toList) ::: nodes
                                case t: Tree =>/* ignore */
                            }
                        }

                        traverse0
                }
            }
            case Nil =>
        }

        traverse0
    }
}
