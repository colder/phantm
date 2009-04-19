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
            case List("variable", "T_INC") => PostInc(expr(child(n, 0)))
            case List("T_INC", "variable") => PreInc(expr(child(n, 1)))
            case List("variable", "T_DEC") => PostDec(expr(child(n, 0)))
            case List("T_DEC", "variable") => PreDec(expr(child(n, 1)))
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
            case List("expr", "T_QUESTION", "expr", "T_COLON", "expr") =>
                Ternary(expr(child(n, 0)), Some(expr(child(n, 2))), expr(child(n, 4)))
            case List("expr", "T_QUESTION", "T_COLON", "expr") =>
                Ternary(expr(child(n, 0)), None, expr(child(n, 2)))
            case List("internal_functions_in_yacc") =>
                internal_functions_in_yacc(child(n, 0))
            case List("T_INT_CAST", "expr") =>
                Cast(CastInt, expr(child(n, 1)))
            case List("T_DOUBLE_CAST", "expr") =>
                Cast(CastDouble, expr(child(n, 1)))
            case List("T_STRING_CAST", "expr") =>
                Cast(CastString, expr(child(n, 1)))
            case List("T_ARRAY_CAST", "expr") =>
                Cast(CastArray, expr(child(n, 1)))
            case List("T_OBJECT_CAST", "expr") =>
                Cast(CastObject, expr(child(n, 1)))
            case List("T_BOOL_CAST", "expr") =>
                Cast(CastBool, expr(child(n, 1)))
            case List("T_UNSET_CAST", "expr") =>
                Cast(CastUnset, expr(child(n, 1)))
            case List("T_EXIT", "exit_expr") =>
                Exit(exit_expr(child(n, 1)))
            case List("T_AT", " expr") =>
                Silence(expr(child(n, 1)))
            case List("scalar") =>
                scalar(child(n,0))
            case List("T_ARRAY", "T_OPEN_BRACES", "array_pair_list", "T_CLOSE_BRACES") =>
                Array(array_pair_list(child(n, 2)))
            case List("T_BACKTICK", "backticks_expr", "T_BACKTICK") =>
                Execute("") // Todo
            case List("T_PRINT", "expr") =>
                Print(expr(child(n, 1)))
            case List("T_FUNCTION", "is_reference", "T_OPEN_BRACES", "parameter_list", "T_CLOSE_BRACES", "lexical_vars", "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") =>
                notyet(n)
            case _ => unspecified(n)
        }
    }

    def internal_functions_in_yacc(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_ISSET", "T_OPEN_BRACES", "isset_variables", "T_CLOSE_BRACES") =>
                Isset(isset_variables(child(n, 2)))
            case List("T_EMPTY", "T_OPEN_BRACES", "variable", "T_CLOSE_BRACES") =>
                Empty(variable(child(n, 2)))
            case List("T_INCLUDE", "expr") =>
                Include(expr(child(n, 1)), false)
            case List("T_INCLUDE_ONCE", "expr") =>
                Include(expr(child(n, 1)), true)
            case List("T_EVAL", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES") =>
                Eval(expr(child(n, 2)))
            case List("T_REQUIRE", "expr") =>
                Require(expr(child(n, 1)), false)
            case List("T_REQUIRE_ONCE", "expr") =>
                Require(expr(child(n, 1)), true)
        }
    }

    def isset_variables(n: ParseNode): List[Variable] = {
        childrenNames(n) match {
            case List("variable") => List(variable(child(n, 0)))
            case List("isset_variables", "T_COMMA", "variable") => isset_variables(child(n, 0)) ::: List(variable(child(n, 2)))
        }
    }

    def array_pair_list(n: ParseNode): List[(Option[Expression], Expression)] = {
        notyet(n);
    }

    def exit_expr(n: ParseNode): Option[Expression] = {
        childrenNames(n) match {
            case List() => None
            case List("T_OPEN_BRACES", "T_CLOSE_BRACES") => None
            case List("T_OPEN_BRACES", "expr", "T_CLOSE_BRACES") => Some(expr(child(n, 1)))
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

    def unspecified(n: ParseNode) = { throw new RuntimeException("Unspecified: "+childrenNames(n).mkString("[", ",", "]")) }
    def notyet(n: ParseNode) = { throw new RuntimeException("Not yet implemented: "+childrenNames(n).mkString("[", ",", "]")) }

}
