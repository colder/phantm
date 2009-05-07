package phpanalysis.analyzer;

import phpanalysis.parser.Trees.Tree;

abstract class ASTTraversal[ContextType](root: Tree, initCtx: ContextType) {
    private var nodes: List[(Tree, ContextType)] = (root, initCtx) :: Nil;

    def elements(p:Product) = (0 until p.productArity).map(p.productElement(_))

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
                addRec(t1, ctx)
                addRec(t2, ctx)
            case (t1, t2, t3) =>
                addRec(t1, ctx)
                addRec(t2, ctx)
                addRec(t3, ctx)
            case ns2: List[_] =>
                ns2 flatMap {n => addRec(n, ctx)}
            case _ =>
                List()
        }
    }

    def visit(node: Tree, ctx: ContextType): ContextType

    def traverse: Unit = nodes match {
        case (node, ctx) :: ns => {
            val newCtx: ContextType = visit(node, ctx)

            nodes = ns

            /* DFS */
            nodes = (elements(node) flatMap { el: Any => addRec(el, newCtx) } toList) ::: nodes

            traverse
        }
        case Nil =>
    }
}
