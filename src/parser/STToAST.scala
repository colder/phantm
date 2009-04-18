// This class transforms a Syntax Tree to an Abstract Syntax Tree
package phpanalysis.parser;

import phpanalysis.parser.Trees._;

import phpanalysis._;

class STToAST(st: ParseNode) {

    def getAST = S(st);

    // Tree visitors functions

    def S(n: ParseNode): Program = new Program(top_statement_list(child(n, 0)))

    def top_statement_list(n: ParseNode): List[Statement] = {
        childrenNames(n) match {
            case List() => Nil
            case List("top_statement_list", "top_statement") => 
                top_statement_list(child(n, 0)) ::: List(top_statement(child(n, 1)))
        }
    }

    def top_statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("statement") => statement(child(n))
            case _ => notyet(n)
        }
    }

    def statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("T_ECHO", "echo_expr_list", "T_SEMICOLON") => Echo(echo_expr_list(child(n, 1)))
            case List("expr", "T_SEMICOLON") => expr(child(n, 0))
            case _ => notyet(n)
        }
    }

    def expr(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("variable", "T_ASSIGN", "expr") => 
                Assign(variable(child(n, 0)), expr(child(n, 2)), false)
            case List("variable", "T_ASSIGN", "T_BITWISE_AND", "variable") => 
                Assign(variable(child(n, 0)), expr(child(n, 2)), false)
            case List("variable", "T_ASSIGN", "T_BITWISE_AND", "T_NEW", "class_name_reference", "ctor_arguments") =>
                notyet(n)
            case List("variable", "T_PLUS_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), Plus(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_MINUS_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), Minus(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_MUL_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), Mult(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_DIV_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), Div(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_CONCAT_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), Concat(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_MOD_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), Mod(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_AND_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), BitwiseAnd(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_OR_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), BitwiseOr(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_XOR_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), BitwiseXor(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_SL_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), ShiftLeft(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_SR_EQUAL", "expr") =>
                Assign(variable(child(n, 0)), ShiftRight(variable(child(n, 0)), expr(child(n, 2))), false)
            case List("variable", "T_INC") => notyet(n)
            case List("T_INC", "variable") => notyet(n)
            case List("variable", "T_DEC") => notyet(n)
            case List("T_DEC", "variable") => notyet(n)
            case List("expr", "T_BOOLEAN_OR", "expr") =>
                BooleanOr(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_BOOLEAN_AND", "expr") =>
                BooleanAnd(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_LOGICAL_OR", "expr") =>
                BooleanOr(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_LOGICAL_AND", "expr") =>
                BooleanAnd(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_LOGICAL_XOR", "expr") =>
                BooleanXor(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_BITWISE_OR", "expr") =>
                BitwiseOr(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_BITWISE_AND", "expr") =>
                BitwiseAnd(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_BITWISE_XOR", "expr") =>
                BitwiseXor(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_POINT", "expr") =>
                Concat(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_PLUS", "expr") =>
                Plus(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_MINUS", "expr") =>
                Minus(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_MULT", "expr") =>
                Mult(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_DIV", "expr") =>
                Div(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_MODULO", "expr") =>
                Mod(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_SL", "expr") =>
                ShiftLeft(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_SR", "expr") =>
                ShiftRight(expr(child(n, 0)), expr(child(n, 2)))
            case List("T_NOT", "expr") =>
                BooleanNot(expr(child(n, 1)))
            case List("T_BITWISE_NOT", "expr") =>
                BitwiseNot(expr(child(n, 1)))
            case List("expr", "T_IS_IDENTICAL", "expr") =>
                Identical(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_NOT_IDENTICAL", "expr") =>
                BooleanNot(Identical(expr(child(n, 0)), expr(child(n, 2))))
            case List("expr", "T_IS_EQUAL", "expr") =>
                Equal(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_NOT_EQUAL", " expr") =>
                BooleanNot(Equal(expr(child(n, 0)), expr(child(n, 2))))
            case List("expr", "T_IS_SMALLER", "expr") =>
                Smaller(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_SMALLER_OR_EQUAL", "expr") =>
                SmallerEqual(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_GREATER", "expr") =>
                BooleanNot(SmallerEqual(expr(child(n, 0)), expr(child(n, 2))))
            case List("expr", "T_IS_GREATER_OR_EQUAL", "expr") =>
                BooleanNot(Smaller(expr(child(n, 0)), expr(child(n, 2))))
            case List("expr", "T_INSTANCEOF", "class_name_reference") =>
                notyet(n)
            case List("T_OPEN_BRACES", "expr", "T_CLOSE_BRACES") =>
                expr(child(n, 1))
            case _ => notyet(n)
        }
    }

    def variable(n: ParseNode): Variable = {
        notyet(n)
    }

    def echo_expr_list(n: ParseNode): List[Expression] = {
        childrenNames(n) match {
            case List("echo_expr_list", "T_COMMA", "expr") => echo_expr_list(child(n, 0)) ::: List(expr(child(n, 2)))
            case List("expr") => List(expr(child(n)))
        }
    }


    // Helper functions

    def child(node: ParseNode): ParseNode = child(node, 0)

    def child(node: ParseNode, num: Int): ParseNode = {
        val cs = node.children();
        if (num > cs.size) {
            throw new RuntimeException("Can't find a child under node \""+(node.name)+"\" at position "+num);
        } else {
            cs.get(num)
        }
    }

    def childrenNames(node: ParseNode): List[String] = {
        var result: List[String] = Nil;
        for (c <- new JavaListIteratorWrapper(node.children().listIterator())) {
            result = result ::: c.name :: Nil
        }
        result
    }

    def notyet(n: ParseNode) = { throw new RuntimeException("Not yet implemented: "+childrenNames(n).mkString("[", ",", "]")) }
    def notyet = throw new RuntimeException("Not yet implemented");

}
