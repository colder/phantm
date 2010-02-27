package phpanalysis.analyzer
import parser.Trees._
import java.io.File
import scala.collection.mutable.Set;

object IncludeResolver {
    val includedFiles = Set[String]();

    var deepNess = 0;

    def begin = {
        deepNess += 1;
    }

    def end = {
        deepNess -= 1;
    }
}

case class IncludeResolver(ast: Program) extends ASTTransform(ast) {

    override def trExpr(ex: Expression): Expression = ex match {
        case Include(path, once) =>
            includeFile(ex, path, once, false)
        case Require(path, once) =>
            includeFile(ex, path, once, true)
        case _ => super.trExpr(ex)
    }


    def includeFile(inc: Expression, path: Expression, once: Boolean, require: Boolean): Expression = {

        def shouldInclude(p: String): Boolean = {
            !once || !IncludeResolver.includedFiles.contains(p)
        }

        def pathExists(p: String): Boolean = new File(p).exists

        def getAST(path: String): Expression = {
            import parser.STToAST

            IncludeResolver.includedFiles += path

            val c = new Compiler(path)
            c compile match {
                case Some(node) =>
                    var ast: Program = new STToAST(c, node) getAST;
                    // We define/resolve constants there too
                    ast = ConstantsResolver(ast, false).transform
                    // We include-resolve this file too
                    ast = IncludeResolver(ast).transform

                    Block(ast.stmts)
                case None =>
                    Reporter.notice("Cannot preprocess \""+path+"\": sub-compilation failed", inc)
                    PHPNull()
            }
        }

        def notfound(p: String): Expression = {
            Reporter.notice("Cannot preprocess \""+p+"\": file not found", inc)
            PHPFalse()
        }

        IncludeResolver.begin

        val result = if (IncludeResolver.deepNess < 20) {
            Evaluator.staticEval(path, false) match {
                case Some(scalar_p) =>
                    val p = Evaluator.scalarToString(scalar_p)
                    if (p(0) == '/') {
                        if (pathExists(p)) {
                            if (shouldInclude(p)) {
                                getAST(p)
                            } else {
                                PHPFalse()
                            }
                        } else {
                            notfound(p)
                        }
                    } else {
                        var foundPath: Option[String] = None;

                        for (prefix <- Main.includePaths if foundPath == None) {
                            val fullpath = prefix+"/"+p;
                            if (pathExists(fullpath)) {
                                foundPath = Some(fullpath)
                            }
                        }

                        foundPath match {
                            case Some(path) =>
                                if (shouldInclude(p)) {
                                    getAST(path)
                                } else {
                                    PHPFalse()
                                }
                            case None =>
                                notfound(p)
                        }
                    }
                case None =>
                    if (Main.verbosity >= 2) {
                        Reporter.notice("Include with non trivial argument will be ignored", path)
                    }
                    PHPFalse()
            }
        } else {
            Reporter.error("Include nesting level too deep: "+IncludeResolver.deepNess, path)
            PHPFalse()
        }

        Reporter.errorMilestone

        IncludeResolver.end

        result
    }

    if (ast.file != None) {
        IncludeResolver.includedFiles += ast.file.get
    } else {
        println("AST pos undef")
    }
}
