package phpanalysis;

import phpanalysis.parser._;
import java.io._;

object ParseTreeGraph {

    def main(args: Array[String]): Unit = {

        if (args.length == 1) {
            new Compiler(args(0)) compile match {
                case Some(node) => {
                    val fileName     = "result.dot";
                    val outputStream = new java.io.FileOutputStream(fileName);
                    val printStream  = new java.io.PrintStream(outputStream);
                    generateDotGraph(node, printStream);
                    println("Dot graph written to "+fileName+".");
                    printStream.close
                }
                case None => println("Compilation failed.");
            }
        } else {
            usage
        }
    }

    def usage = {
        println("Usage: phpanalysis <file>");
    }

    def generateDotGraph(root: ParseNode, printStream: java.io.PrintStream) {
        var nextId = 1;

        def emit(str: String) = printStream.print(str);

        def getId = {val id = nextId; nextId = nextId + 1; id }

        def dotPrint(node: ParseNode, id: Int): Unit = {
            emit("  node"+id+"[label=\""+(node.name())+"\"]\n");
            for (c <- new JavaListIteratorWrapper(node.children().listIterator())) {
                val cid = getId
                dotPrint(c, cid)
                emit("  node"+id+" -> node"+cid+"\n");
            }
        }

        emit("digraph D {\n");
        dotPrint(root, getId);
        emit("}\n");
    }

}
