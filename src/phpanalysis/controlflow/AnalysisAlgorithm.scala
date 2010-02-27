package phpanalysis.controlflow

import scala.collection.mutable.Set;

class AnalysisAlgorithm[E <: Environment[E],S]
               (transferFun : TransferFunction[E,S],
                bottomEnv : E,
                baseEnv : E,
                cfg : LabeledDirectedGraphImp[S])
{
    type Vertex = VertexImp[S]

    var facts : Map[Vertex, E] = Map[Vertex,E]()

    def init = {
        facts = Map[Vertex,E]().withDefaultValue(bottomEnv)
    }

    def pass(transferFun: TransferFunction[E,S]) = {
        for (v <- cfg.V) {
            for (e <- cfg.inEdges(v)) {
                transferFun(e.lab, facts(e.v1))
            }
        }
    }

    def computeFixpoint : Unit = {
        var pass = 0;

        var workList = Set[Vertex]();

        if (Main.displayProgress) {
            println("      * Analyzing CFG ("+cfg.V.size+" vertices, "+cfg.E.size+" edges)")
        }

        facts(cfg.entry) = baseEnv

        for (v <- cfg.V) {
            workList += v
        }

        while (workList.size > 0) {
            pass += 1

            if (Main.displayProgress) {
              println("      * Pass "+pass+" ("+workList.size+" nodes to propagate)...")
            }

            val passWorkList = Set[Vertex]() ++ workList;
            workList = Set[Vertex]()

            for(v <- passWorkList) {
                val oldFact : E = facts(v)
                var newFact : Option[E] = None

                for (e <- cfg.inEdges(v) if facts(e.v1) != bottomEnv) {
                    val propagated = transferFun(e.lab, facts(e.v1));

                    newFact = newFact match {
                        case Some(nf) => Some(nf union propagated)
                        case None => Some(propagated)
                    }
                }

                val nf = newFact.getOrElse(baseEnv);

                if (nf != oldFact) {
                    println("["+v+"]: ")
                    println("  OLD: "+oldFact)
                    println("  NEW: "+nf)

                    facts = facts.update(v, nf)

                    for (e <- cfg.outEdges(v)) {
                        workList += e.v2;
                    }
                }

            }
        }
    }

    def dumpFacts = {
    for ((v,e) <- facts.toList.sort{(x,y) => x._1.name < y._1.name}) {
        println("  "+v+" => "+e)
    }
    }
    def getResult : Map[Vertex,E] = facts
}
