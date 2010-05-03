package phantm.dataflow

import phantm.Settings
import phantm.phases.PhasesContext
import phantm.cfg.{LabeledDirectedGraphImp, VertexImp}

class AnalysisAlgorithm[E <: Environment[E, S],S]
               (transferFun : TransferFunction[E,S],
                bottomEnv : E,
                baseEnv : E,
                cfg : LabeledDirectedGraphImp[S])
{
    type Vertex = VertexImp[S]

    var facts : Map[Vertex, E] = Map[Vertex,E]().withDefaultValue(bottomEnv)

    def pass(transferFun: TransferFunction[E,S]) = {
        for (v <- cfg.V) {
            for (e <- cfg.inEdges(v)) {
                // We ignore unreachable code
                if (facts(e.v1) != bottomEnv) {
                    transferFun(e.lab, facts(e.v1))
                }
            }
        }
    }

    def detectUnreachable(transferFun: TransferFunction[E,S]): List[S] = {
        var res : List[S] = Nil;

        for (v <- cfg.V if v != cfg.entry) {
            if (cfg.inEdges(v).forall(e => (facts(e.v1) != bottomEnv) &&
                                           (transferFun(e.lab, facts(e.v1)) == bottomEnv))) {

                for (e <- cfg.outEdges(v)) {
                    res = e.lab :: res
                }
            }
        }

        res
    }

    def computeFixpoint(ctx: PhasesContext) : Unit = {
        var pass = 0;

        var workList = Set[Vertex]();

        if (Settings.get.displayProgress) {
            println("      * Analyzing CFG ("+cfg.V.size+" vertices, "+cfg.E.size+" edges)")
        }

        facts = facts.updated(cfg.entry, baseEnv)

        for (e <- cfg.outEdges(cfg.entry)) {
            workList += e.v2
        }

        while (workList.size > 0) {
            pass += 1

            if (Settings.get.displayProgress) {
              println("      * Pass "+pass+" ("+workList.size+" nodes to propagate)...")
            }



            val passWorkList = Set[Vertex]() ++ workList;
            workList = Set[Vertex]()

            for(v <- passWorkList) {

                //println("[[[["+v+"]]]]")
                //println("Facts: "+facts(v))
                val oldFact : E = facts(v)
                var newFact : Option[E] = None

                for (e <- cfg.inEdges(v) if facts(e.v1) != bottomEnv) {
                    //println("##>"+e.lab+"("+facts(e.v1)+")")
                    val propagated = transferFun(e.lab, facts(e.v1));
                    //println("##=>"+propagated)

                    if (propagated != bottomEnv) {
                        newFact = newFact match {
                            case Some(nf) => Some(nf union propagated)
                            case None => Some(propagated)
                        }
                    }
                }

                val nf = newFact.getOrElse(oldFact.copy);

                if (nf != oldFact) {
                    if (Settings.get.testsActive) {
                        oldFact.checkMonotonicity(v, nf, ctx, cfg.inEdges(v) map (e => (e.lab, facts(e.v1))))
                    }

                    //println("@@ Updating facts to "+nf)
                    facts = facts.updated(v, nf)

                    for (e <- cfg.outEdges(v)) {
                        workList += e.v2;
                    }
                }

            }
        }
    }

    def dumpFacts = {
        for ((v,e) <- facts.toList.sortWith{(x,y) => x._1.name < y._1.name}) {
            println("  "+v+" => "+e)
        }
    }
    def getResult : Map[Vertex,E] = facts
}
