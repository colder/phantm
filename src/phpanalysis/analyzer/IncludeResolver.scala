package phpanalysis.analyzer
import parser.Trees._
import java.io.File
import scala.io.Source
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

    var inclsInstr = Map[(String, Int), Set[String]]();

    def importIncludes(files: List[String]) = {
        for (incl <- files) {
            for (l <- Source.fromFile(new File(incl)).getLines) {
                var chars = l.toList

                def consumeInt : Int = {
                    var buf = "";
                    while(chars.head >= '0' && chars.head <= '9') {
                        buf += chars.head
                        chars = chars.tail
                    }

                    buf.toInt
                }

                val size = consumeInt
                chars = chars.tail // :
                val file = chars.take(size).mkString
                chars = chars.drop(size+1); // path:
                val line = consumeInt
                chars = chars.tail // :
                val pathsize = consumeInt
                chars = chars.tail // :
                val path = chars.take(pathsize).mkString

                inclsInstr += ((file, line) -> (inclsInstr.getOrElse((file, line), Set[String]()) + path))

            }
        }
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

        def pathExists(p: String): Option[String] = {
            val f = new File(p)
            if (f.exists) {
                Some(f.getAbsolutePath)
            } else {
                None
            }
        }

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
            val eval = Evaluator.staticEval(path, false)
            val pathres = (eval, path) match {
                case (Some(scal), _) =>
                    Some(scal)
                case (None, fc @ FunctionCall(StaticFunctionRef(_, _, Identifier("phantm_incl")), _)) =>
                    // probably instrumentalized, let's check if the position can be found
                    val absPath = if (inc.file.isEmpty) "?" else new File(inc.file.get).getAbsolutePath;
                    IncludeResolver.inclsInstr.get((absPath, inc.line)) match {
                        case Some(paths) =>
                            if (paths.size > 1) {
                                if (Main.verbosity >= 0) {
                                    Reporter.notice("Include statement including more than one file!", inc)
                                }
                            }
                            Some(PHPString(paths.toList.head).setPos(fc))
                        case None =>
                            None
                    }
                case _ =>
                    None
            }
            pathres match {
                case Some(scalar_p) =>
                    val p = Evaluator.scalarToString(scalar_p)
                    if (p(0) == '/') {
                        val realpath = pathExists(p);
                        if (!realpath.isEmpty) {
                            if (shouldInclude(realpath.get)) {
                                getAST(realpath.get)
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
                            val realpath = pathExists(fullpath);
                            if (!realpath.isEmpty) {
                                foundPath = Some(realpath.get)
                            }
                        }

                        foundPath match {
                            case Some(path) =>
                                if (shouldInclude(path)) {
                                    getAST(path)
                                } else {
                                    PHPFalse()
                                }
                            case None =>
                                notfound(p)
                        }
                    }
                case None =>
                    if (Main.verbosity >= 0) {
                        Reporter.notice("Include with non trivial argument will be ignored", inc)
                    }
                    PHPFalse()
            }
        } else {
            Reporter.error("Include nesting level too deep: "+IncludeResolver.deepNess, inc)
            PHPFalse()
        }

        Reporter.errorMilestone

        IncludeResolver.end

        result
    }

    if (ast.file != None) {
        IncludeResolver.includedFiles += ast.file.get
    }
}
