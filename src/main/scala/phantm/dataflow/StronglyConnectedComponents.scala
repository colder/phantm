package phantm.dataflow

import phantm.cfg._

class StronglyConnectedComponents[S](cfg: LabeledDirectedGraphImp[S]) {
    type Vertex = VertexImp[S]
    type Edge   = EdgeImp[S]

    class Node(var v: Vertex, var index: Int, var vindex: Int, var lowlink: Int, var caller: Option[Node], var vSeq: IndexedSeq[Vertex]) {
       override def toString = v.toString
    }

    class SCC(val vs: Set[Vertex], var adjSCC: Set[SCC])

    def getComponents = {
        var sccs     = Set[SCC]()
        var vToScc   = Map[Vertex, SCC]()

        var onStack  = Set[Node]()
        var stack    = List[Node]()
        var index    = 0

        var nodes    = Map[Vertex, Node]()

        def push(n: Node) = {
            stack = n :: stack
            onStack += n
        }

        def pop: Node = {
            val n = stack.head
            stack = stack.tail
            onStack -= n
            n
        }

        def tarjan(v: Vertex): Unit = {
            val n1= new Node(v, index, 0, index, None, cfg.outEdges(v).toIndexedSeq.map((e: Edge) => e.v2))
            nodes += v -> n1
            index += 1
            push(n1)

            var last = n1
            var continue = true

            while (continue) {
                if (last.vindex < last.vSeq.size) {
                    val nv = last.vSeq(last.vindex)
                    last.vindex += 1;

                    val optN = nodes.get(nv)
                    if (optN == None) {
                        val n2 = new Node(nv, index, 0, index, Some(last), cfg.outEdges(nv).toIndexedSeq.map((e: Edge) => e.v2))
                        nodes += nv -> n2
                        index += 1
                        push(n2)
                        last = n2
                    } else if (onStack contains optN.get) {
                        last.lowlink = last.lowlink.min(optN.get.index)
                    }

                } else {
                    if (last.lowlink == last.index) {
                        // SCC
                        var set = Set[Vertex]()
                        var top = stack.head
                        set += pop.v

                        while(top != last) {
                            top = pop
                            set += top.v
                        }
                        val scc = new SCC(set, Set())

                        set.foreach(vToScc += _ -> scc)

                        sccs += scc
                    }

                    val optCaller = last.caller
                    if (optCaller == None) {
                        continue = false
                    } else {
                        optCaller.get.lowlink = optCaller.get.lowlink.min(last.lowlink)
                        last = optCaller.get
                    }
                }
            }

        }


        var vertices = cfg.V.toSeq
        while(!vertices.isEmpty) {
            val node = vertices.head
            vertices = vertices.tail

            if (!(nodes contains node)) {
                tarjan(node)
            }
        }

        // be blunt, traverse again to get adjacent SCC
        for(scc <- sccs) {
            scc.adjSCC = scc.vs.flatMap(v => cfg.outEdges(v).map(e => vToScc(e.v2)).filter(_ != scc)).toSet
        }

        sccs
    }

    def topSort(sccs: Set[SCC]): Map[Vertex, Int] = {
        // first we get the roots
        var todo = (sccs &~ sccs.flatMap(scc => scc.adjSCC)).toSeq

        var pos = 0;
        var wl = Map[Vertex, Int]()

        while (todo.size > 0) {
            val scc = todo.head
            for (v <- scc.vs) {
                wl += v -> pos
            }
            pos += 1
            todo = todo.tail

            for (s <- scc.adjSCC) {
                todo = todo :+ s
            }
        }

        wl
    }

}

 
