package phpanalysis.helpers;

import phpanalysis.parser._;
import java.io._;

class ASTGraph extends Helper {

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
            new Compiler(input) compile match {
                case Some(node) =>
                    generateDotGraph(STToAST(node).getAST, printStream)
                    printStream.close
                case None =>
                    throw new Exception("Compilation failed");

            }
    }

    private def generateDotGraph(root: Trees.Program, printStream: java.io.PrintStream) {
        import phpanalysis.parser.Trees._;
        var nextId = 1;

        def emit(str: String) = printStream.print(str);

        def getId = {val id = nextId; nextId = nextId + 1; id }

        def getLabel(node: Tree) = {
            val str = node.getClass.toString;
            if (str.split("\\$").length > 0) {
                str.split("\\$").last
            } else {
                str
            }
        }

        def escape(s: String) = s.replaceAll("\"", "")

        def elements(p:Product) = (0 until p.productArity).map(p.productElement(_))

        def dotPrint(o: Any, pid: Int): Unit = {
            import phpanalysis.parser.Trees._;
            o match {
                case Some(n) => dotPrint(n, pid)
                case None =>

                case _ => {
                    val id = getId
                    if (pid > 0) emit("  node"+pid+" -> node"+id+"\n");
                    o match {
                        case node: Tree =>  {
                            emit("  node"+id+"[label=\""+getLabel(node)+"\"]\n");

                            for(c <- elements(node)) dotPrint(c, id)
                        }

                        case ns: List[_] => {
                            emit("  node"+id+"[label=\"<List>\"]\n");
                            ns map {n => dotPrint(n, id)}
                        }

                        case i: Int => {
                            emit("  node"+id+"[label=\"<Int: "+i+">\"]\n");
                        }

                        case s: String => {
                            emit("  node"+id+"[label=\"<String: "+escape(s)+">\"]\n");
                        }

                        case b: Boolean => {
                            emit("  node"+id+"[label=\"<Bool: "+b+">\"]\n");
                        }

                        case _ => {
                            emit("  node"+id+"[label=\"<Unknown>\"]\n");
                        }
                    }
                }


            }
        }

        emit("digraph D {\n");
        dotPrint(root, 0);
        emit("}\n");
    }

}
