package phpanalysis.analyzer
import parser.Trees._
import java.io.File

case class IncludeResolver(ast: Program) extends ASTTransform(ast) {

    override def trExpr(ex: Expression): Expression = ex match {
        case Include(path, once) =>
            includeFile(ex, path, once, false)
        case Require(path, once) =>
            includeFile(ex, path, once, true)
        case _ => super.trExpr(ex)
    }

    def includeFile(inc: Expression, path: Expression, once: Boolean, require: Boolean): Expression = {
        def dirname(path: String): String = {
            val ind = path.lastIndexOf('/')
            if (ind < 0) {
                "."
            } else {
                path.substring(0, ind)
            }
        }

        def unrollPath(path: Expression): Option[String] = path match {
            case Concat (lhs, rhs) =>
                (unrollPath(lhs), unrollPath(rhs)) match {
                    case (Some(slhs), Some(srhs)) => Some(slhs+srhs)
                    case _ => None
                }
            case FunctionCall(StaticFunctionRef(_,_,Identifier("dirname")), List(CallArg(arg, _))) =>
                unrollPath(arg) match {
                    case Some(a) =>
                        Some(dirname(a))
                    case None =>
                        None
                }
            case Constant(_) =>
                Some("CONSTANT")
            case ClassConstant(_:StaticClassRef, _) =>
                Some("CLASSCONSTANT")
            case PHPTrue() =>
                Some("1")
            case PHPFalse() =>
                Some("")
            case PHPInteger(value) =>
                Some(""+value)
            case PHPFloat(value) =>
                Some(""+value)
            case PHPString(value) =>
                Some(value)
            case PHPNull() =>
                Some("")
            case MCFile() =>
                inc.file match {
                    case Some(p) =>
                        Some(new File(p).getAbsolutePath())
                    case None =>
                        Some("")
                }
            case MCLine() =>
                Some(""+inc.line)
            case MCDir() =>
                inc.file match {
                    case Some(p) =>
                        Some(dirname(new File(p).getAbsolutePath()))
                    case None =>
                        Some("")
                }
            case sc: Scalar =>
                Some(""+sc)
            case _ =>
                None
        }

        def pathExists(p: String): Boolean = new File(p).exists

        def getAST(path: String): Expression = {
            import parser.STToAST
            new Compiler(path) compile match {
                case Some(node) =>
                    var ast: Program = new STToAST(node) getAST;
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

        unrollPath(path) match {
            case Some(p) =>
                if (p(0) == '/') {
                    if (pathExists(p)) {
                        getAST(p)
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
                            getAST(path)
                        case None =>
                            notfound(p)
                    }
                }
            case None =>
                Reporter.notice("Include with non trivial argument will be ignored", path)
                PHPFalse()
        }
    }
}
