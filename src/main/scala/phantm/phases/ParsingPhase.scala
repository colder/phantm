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
    import cup2.generator._
    import cup2.parser._
    import cup2.scanner._
    import cup2.grammar.Terminal
    import cup2.grammar.SpecialTerminals.EndOfInputStream


    class JFlexToCUP(val l: Lexer) extends Scanner {
      def readNextTerminal: ScannerToken[String] = {
        val yytoken = l.lex();

        if (yytoken ne null) {
          new ScannerToken(yytoken.tpe.asInstanceOf[Terminal], yytoken.content, yytoken.line, yytoken.column)
        } else {
          new ScannerToken(EndOfInputStream)
        }
      }
    }

    val table = new LR1Generator(new PHP53Spec()).getParsingTable();

    for (f <- ctx.files) {
      val l = new Lexer(new java.io.FileReader(f));
      l.setFileName(f);

      val parser = new LRParser(table)

      val res = parser.parse(new JFlexToCUP(l))

      println(res)
      
    }

    ctx.copy(oast = None)
  }
}
