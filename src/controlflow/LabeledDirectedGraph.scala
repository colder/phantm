package phpanalysis.controlflow
 
/** Mutable Directed Graph with Labels */
abstract trait LabeledDirectedGraph[LabelType] {
  /** The type of the vertices */
  type Vertex
  /** The type of the edges */
  type Edge
  /** The vertices */
  def V: Set[Vertex]
  /** The edges */
  def E: Set[Edge]
  /** Adds a new vertex to the graph */
  def newVertex: Vertex
  /** Adds a new labeled edge between two vertices */
  def += (from: Vertex, lab: LabelType, to: Vertex)
  /** Returns the set of incoming edges for a given vertex */
  def inEdges(v: Vertex): Set[Edge]
  /** Returne the set of outgoing edges for a given vertex */
  def outEdges(v: Vertex): Set[Edge]
  /** Removes an edge from the graph */
  def -= (from: Vertex, lab: LabelType, to: Vertex)
  /** Generates a new, isomorphic labeled directed graphs where the new labels
   * are obtained by mapping the function to the existing ones */
  def labelMap[NewLabel](f: LabelType => NewLabel): LabeledDirectedGraph[NewLabel]
}


case class VertexImp[L](var name: String) {
  var in: Set[EdgeImp[L]] = Set[EdgeImp[L]]()
  var out: Set[EdgeImp[L]] = Set[EdgeImp[L]]()
  override def toString = name
}

case class EdgeImp[L](v1: VertexImp[L], lab: L, v2: VertexImp[L]) {
  val name = EdgeCounter.getNew
  override def toString = name + ":" + v1 + "-" + lab + "-" + v2
}

object EdgeCounter {
  var count = 0
  def getNew: String = {
    count = count + 1
    ("e" + count)
  }
}
 
/** A concrete yet type-paremetric implementation */
class LabeledDirectedGraphImp[LabelType] extends LabeledDirectedGraph[LabelType] {
 
  type Vertex = VertexImp[LabelType]
  type Edge = EdgeImp[LabelType]
  
  private var vertices = Set[Vertex]()
  private var edges = Set[Edge]()

  def V = vertices
  def E = edges
 
  var counter = 0
  def newVertex = { 
    counter = counter + 1
    new Vertex("v" + counter)
  }
  
  def +=(from: Vertex, lab : LabelType, to: Vertex) = {
    val edge = EdgeImp[LabelType](from, lab, to)
    edges += edge
    vertices += from
    vertices += to
    from.out += edge
    to.in += edge
  }
  
  def -=(from: Vertex, lab: LabelType, to: Vertex) = {
    val edge = EdgeImp[LabelType](from, lab, to)
    edges -= edge
    vertices -= from
    vertices -= to
    from.out -= edge
    to.in -= edge
  }
 def escape(s: String) = s.replaceAll("\\\\", "\\\\\\\\").replaceAll("\"", "\\\\\"").replaceAll("\\\n", "\\\\n")

  private var groups: List[Group] = Nil;
  private var groupStack: List[Group] = Nil;

  private var groupN: Int = 0;

  case class Group(name: String, vertIn: Vertex) {
     val n = groupN
     groupN = groupN + 1
     var vertOut: Option[Vertex] = None
     var subgroups: List[Group] = List();

     def newSubGroup(name: String, vertIn: Vertex) : Group = {
        subgroups = Group(name, vertIn)::subgroups
        subgroups.head
     }

     def closeGroup(vertOut: Vertex) = {
        this.vertOut match {
            case None => this.vertOut = Some(vertOut)
            case Some(x) => error("Group already closed")
        }
     }


     def toDotString(res: StringBuffer) : Unit = {
         val colors = List("bisque", "khaki", "mistyrose", "lightcyan", "mediumorchid", "aquamarine", "antiquewhite")
         def emit(s: String) = res.append(s)
        
         if (!vertices.contains(vertIn)) {
            return;
         }

         emit("""
    subgraph cluster"""+n+""" {
        node [style=filled, color=white];
        style=filled;
        labeljust=l;
        label="""+escape(name)+""";
        color=""");

         val colornumber: String = if((n/colors.size)%3 == 0) "" else ((n/colors.size)%3)+"";
         emit(colors(n%colors.size)+colornumber+";\n")

         var alreadyIn: Set[Vertex] = Set[Vertex]()
         val myVertOut = vertOut match {
            case Some(x) => x
            case None => error("Non-closed group");
         }
         alreadyIn += myVertOut


         def followGraph(v:Vertex): Unit = {
            if (!alreadyIn.contains(v) && vertices.contains(v)) {
                alreadyIn += v;
                emit("    "+v.name+";\n");
                for(eOut <- v.out) {
                    emit("    "+eOut.name+";\n");
                    followGraph(eOut.v2);
                }
            }
         }

         
         followGraph(vertIn);
         emit("    "+myVertOut.name)

         subgroups.foreach(sg => sg.toDotString(res))

         emit("    }")
     }
  }

  def openGroup(name: String, vertIn: Vertex) = {
    if (groupStack.size == 0) {
        val group = Group(name, vertIn)
        groups = group::groups
        groupStack = group::groupStack
    } else {
        val group = groupStack.head.newSubGroup(name, vertIn)
        groupStack = group::groupStack
    }
  }

  def closeGroup(vertOut: Vertex) = {
    groupStack.head.closeGroup(vertOut)
    groupStack = groupStack.tail;
  }
 
  def inEdges(v: Vertex)  = v.in
  def outEdges(v: Vertex) = v.out
 
  def labelMap[NewLabel](f: LabelType => NewLabel): LabeledDirectedGraphImp[NewLabel] = {
    val g = new LabeledDirectedGraphImp[NewLabel]
    var vertexMap = Map[Vertex, g.Vertex]()
    for (v <- vertices) {
      vertexMap += ((v, g.newVertex))
    }
    for (e <- edges) {
      g += (vertexMap(e.v1), f(e.lab), vertexMap(e.v2))
    }
    g
  }
 
  /** For visualization purposes. */
  override def toString: String = edges.toList.map(_.toString).mkString("{ ", ", ", " }")
  
  /** The following method prints out a string readable using GraphViz. */
  def toDotString(title: String): String = {
    var res: StringBuffer = new StringBuffer()
    def emit(s: String) = res.append(s)
    def arrow(x: String, y: String) = {
      emit("  "); emit(x); emit(" -> "); emit(y); emit(";\n")
    }
    
    def makeBoxed(id : String, name : String) = {
      emit(id); emit("[shape=box,color=lightblue,style=filled,label=\""); 
      emit(escape(name)); emit("\"];\n")
    }
 
    emit("digraph D {\n")
    emit(" label=\""+title+"\"\n")
    emit(" entry [color=darkolivegreen1,style=filled];\n")
    emit(" exit [color=orangered1,style=filled];\n")

    edges.foreach(edge => {
      arrow(edge.v1.name, edge.name)
      arrow(edge.name, edge.v2.name)
      makeBoxed(edge.name, edge.lab.toString)
    })

    // Display clusters (To use without fewerSkips)
    groups.foreach(x => x.toDotString(res))

    emit("}\n") 
    res.toString
  }
 
  /** Writes the graph to a file readable with GraphViz. */
  def writeDottyToFile(fname: String, title: String): Unit = {
    import java.io.{BufferedWriter, FileWriter}
    val out = new BufferedWriter(new FileWriter(fname))
    out.write(toDotString(title))
    out.close
  }

  private def exec(command: String): Unit = Runtime.getRuntime.exec(command)
}
