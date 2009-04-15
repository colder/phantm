case class Program(stmts: List[Statement]);

abstract class Statement;

case class Block(stmts: List[Statement]) extends Statement
case class If(cond: Expression, then: Statement, elze: Statement) extends Statement
case class While(cond: Expression, then: Statement) extends Statement
case class DoWhile(cond: Expression, then: Statement) extends Statement
case class For(init: List[Expression], cond: List[Expression], step: List[Expression], then: Statement) extends Statement
case class Switch(expr: Expression, cases: List[(Expression, List[Statement])], default: Option[List[Statement]]) extends Statement
case class Break(level: Expression) extends Statement
case class Continue(level: Expression) extends Statement
case class Return(expr: Expression) extends Statement
case class Global(vars: List[Variable]) extends Statement
case class Static(vars: List[Variable]) extends Statement
case class Echo(exprs: List[Expression]) extends Statement
case class Html(content: String) extends Statement
case class Unset(vars: List[Variable]) extends Statement
case class Foreach(what: Expression, as: Variable, key: Option[Variable], body: Statement) extends Statement

abstract class Expression;
