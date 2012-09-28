package phantm.util
import phantm.Settings
import phantm.parser.Parser
import phantm.phases.PhasesContext
import phantm.ast.Trees._
import phantm.ast.ASTTransform

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


case class IncludeResolver(ast: Program, ctx: PhasesContext) extends ASTTransform(ast) {

    override def trExpr(ex: Expression): Expression = ex match {
        case Include(path, once) =>
            includeFile(ex, path, once, false)
        case Require(path, once) =>
            includeFile(ex, path, once, true)
        case _ => super.trExpr(ex)
    }


    def includeFile(inc: Expression, path: Expression, once: Boolean, require: Boolean): Expression = {

        def shouldInclude(p: String, pos: Positional): Boolean = {
            if (!Settings.get.resolveIncludes) {
                Reporter.notice("Include resolution is specifically disabled", pos)
                false
            } else {
                !once || !IncludeResolver.includedFiles.contains(p)
            }
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
            import phantm.ast.STToAST

            IncludeResolver.includedFiles += path

            val p = new Parser(path)
            p parse match {
                case Some(node) =>
                    var ast: Program = new STToAST(p, node) getAST;
                    // We define/resolve constants there too
                    ast = ConstantsResolver(ast, false, ctx).transform
                    // We include-resolve this file too
                    ast = IncludeResolver(ast, ctx).transform

                    Block(ast.stmts).setPos(inc)
                case None =>
                    Reporter.notice("Cannot preprocess \""+path+"\": sub-compilation failed", inc)
                    VoidExpr().setPos(inc)
            }
        }

        def notfound(p: String): Expression = {
            Reporter.notice("Cannot preprocess \""+p+"\": file not found", inc)
            VoidExpr().setPos(inc)
        }

        IncludeResolver.begin

        val result = if (IncludeResolver.deepNess < 20) {
            val eval = Evaluator.staticEval(path, ctx, false)
            val pathres = (eval, path) match {
                case (Some(scal), _) =>
                    (false, Some(List(scal)))
                case (None, fc @ FunctionCall(StaticFunctionRef(_, _, Identifier("phantm_incl")), _)) =>
                    // probably instrumentalized, let's check if the position can be found
                    val absPath = if (inc.file.isEmpty) "?" else new File(inc.file.get).getAbsolutePath;
                    IncludeResolver.inclsInstr.get((absPath, inc.line)) match {
                        case Some(paths) =>
                            (true, Some(paths.map(p => PHPString(p).setPos(fc)).toList))
                        case None =>
                            if (Settings.get.verbosity >= 2) {
                                Reporter.notice("No runtime information found for this include location", inc)
                            }
                            (true, None)
                    }
                case _ =>
                    (false, None)
            }
            pathres match {
                case (_, Some(paths)) =>
                    def astFromScalar(scalar: Scalar): Expression = {
                        val p = Evaluator.scalarToString(scalar)
                        if (p(0) == '/') {
                            val realpath = pathExists(p);
                            if (!realpath.isEmpty) {
                                if (shouldInclude(realpath.get, scalar)) {
                                    getAST(realpath.get)
                                } else {
                                    VoidExpr().setPos(inc)
                                }
                            } else {
                                notfound(p)
                            }
                        } else {
                            var foundPath: Option[String] = None;

                            for (prefix <- Settings.get.includePaths if foundPath == None) {
                                val fullpath = prefix+"/"+p;
                                val realpath = pathExists(fullpath);
                                if (!realpath.isEmpty) {
                                    foundPath = Some(realpath.get)
                                }
                            }

                            foundPath match {
                                case Some(path) =>
                                    if (shouldInclude(path, scalar)) {
                                        getAST(path)
                                    } else {
                                        VoidExpr().setPos(inc)
                                    }
                                case None =>
                                    notfound(p)
                            }
                        }
                    }
                    val asts = paths map (astFromScalar _)

                    if (asts.size > 1) {
                        Alternatives(asts).setPos(inc)
                    } else {
                        asts.head
                    }
                case (instr, None) =>
                    if (Settings.get.verbosity >= 0 && !instr) {
                        Reporter.notice("Include with non trivial argument will be ignored", inc)
                    }
                    VoidExpr().setPos(inc)
            }
        } else {
            Reporter.error("Include nesting level too deep: "+IncludeResolver.deepNess, inc)
            VoidExpr().setPos(inc)
        }

        IncludeResolver.end

        result
    }

    if (ast.file != None) {
        IncludeResolver.includedFiles += ast.file.get
    }
}
