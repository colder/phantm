package phpanalysis.analyzer
import parser.Trees._

case class IncludeResolver(ast: Program) extends ASTTransform(ast) {

    override def trExpr(ex: Expression): Expression = ex match {
        case Include(path, once) =>
            println("Expanding include: "+path+"!")
            ex
        case Require(path, once) =>
            println("Expanding require: "+path+"!")
            ex
        case _ => ex
    }
}
