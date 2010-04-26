package phantm.helpers

import phantm.parser._
import phantm.util.JavaListIteratorWrapper
import java.io._

class STGraph extends Helper {

    def generate(input: String, printStream: java.io.PrintStream): Unit = {
            new Parser(input) parse match {
                case Some(node) =>
                    generateDotGraph(node, printStream)
                    printStream.close
                case None =>
                    throw new Exception("Compilation failed");

            }
    }

    private def generateDotGraph(root: ParseNode, printStream: java.io.PrintStream) {
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
