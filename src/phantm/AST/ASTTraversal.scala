package phantm.AST;

import Trees.Tree;

abstract class ASTTraversal[ContextType](root: Tree, initCtx: ContextType) {

    def elements(p: Product) = (0 until p.productArity).map(p.productElement(_))

    /* Reduces from different structures to Trees */
    def addRec(el: Any, ctx: ContextType): List[(Tree, ContextType)] = {
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

    def traverse (visit: (Tree, ContextType) => (ContextType, Boolean)): Unit = {
        var nodes: List[(Tree, ContextType)] = (root, initCtx) :: Nil;

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
