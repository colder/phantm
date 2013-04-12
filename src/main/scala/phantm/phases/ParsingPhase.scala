package phantm.phases

import edu.tum.cup2

import phantm.parser.Parser
import phantm.ast.STToAST

object ParsingPhase extends Phase {
    def name = "Parsing"
    def description = "Generating AST"

    def run(ctx: PhasesContext): PhasesContext = {

        val sts = ctx.files map { f => val c = new Parser(f); (f, c, c parse) }

        val er = sts find { _._3 == None };

        if (er != None) {
            val e = er.get
            throw PhaseException(this, "Parsing  of '"+e._1+"' failed")
        }

        val asts = sts map { c => new STToAST(c._2, c._3.get) getAST }

        val ast = asts.reduceLeft {(a,b) => a combine b}

        ctx.copy(oast = Some(ast))
    }
}

object ParsingPhaseNew extends Phase {
  def name = "Parsing new"
  def description = "Generating ASTs"

  def run(ctx: PhasesContext): PhasesContext = {
    import phantm.parser.PHP53Spec
    import phantm.parser.Lexer
    import phantm.ast.Trees.Program
    import cup2.generator._
    import cup2.parser._
    import cup2.scanner._
    import cup2.grammar.Terminal
    import cup2.grammar.SpecialTerminals.EndOfInputStream


    class JFlexToCUP(val l: Lexer) extends Scanner {
      def readNextTerminal: ScannerToken[_] = {
        val token = l.lex();

        val res = if (token ne null) {
          token
        } else {
          new ScannerToken(EndOfInputStream)
        }

        println("Read: "+res)

        res
      }
    }

    val table = new LALR1Generator(new PHP53Spec()).getParsingTable();

    val asts = for (f <- ctx.files) yield {
      val l = new Lexer(new java.io.FileReader(f));
      l.setFileName(f);

      val parser = new LRParser(table)

      val res = parser.parse(new JFlexToCUP(l))

      res.asInstanceOf[Program]
    }

    val prog = asts.reduceLeft {(a,b) => a combine b}

    ctx.copy(oast = Some(prog))
  }
}
