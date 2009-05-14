package phpanalysis.analyzer;

import phpanalysis.parser.Trees._;
import phpanalysis.analyzer.Symbols._;

case class Context(varScope: Scope);

case class CollectSymbols(node: Tree) extends ASTTraversal[Context](node, Context(GlobalSymbols)) with Reporter {

    /**
     * Visit the nodes and aggregate information inside the context to provide
     * hints about obvious errors directly from the AST
     */
    def visit(node: Tree, ctx: Context): Context = {
        var newCtx = ctx;

        node match {
            case FunctionDecl(name, args, retref, body) =>
                val fs = new FunctionSymbol(name.value).setPos(name)
                for (val a <- args) {
                    val vs = new VariableSymbol(a.v.name.value).setPos(a.v)
                    fs.registerArgument(vs, a.byref);
                }
                GlobalSymbols.registerFunction(fs)
                newCtx = Context(fs)


            case ClassDecl(name, flags, parent, interfaces, methods, static_props, props, consts) => 
                val cs = new ClassSymbol(name.value, None, Nil).setPos(name);
                GlobalSymbols.registerClass(cs);


            case SimpleVariable(id) =>
                val vs = new VariableSymbol(id.value).setPos(id)
                ctx.varScope.registerVariable(vs)
            case _ =>
        }

        newCtx
    }
}
