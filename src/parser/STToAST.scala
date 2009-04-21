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
            case List("function_declaration_statement") => function_declaration_statement(child(n))
            case List("class_declaration_statement") => class_declaration_statement(child(n))
            case List("T_HALT_COMPILER", "T_OPEN_BRACES", "T_CLOSE_BRACES", "T_SEMICOLON") => notyet(n)
            case List("T_NAMESPACE", "namespace_name", "T_SEMICOLON") => notyet(n)
            case List("T_NAMESPACE", "namespace_name", "T_OPEN_CURLY_BRACES", "top_statement_list", "T_CLOSE_CURLY_BRACES") => notyet(n)
            case List("T_NAMESPACE", "T_OPEN_CURLY_BRACES", "top_statement_list", "T_CLOSE_CURLY_BRACES") => notyet(n)
            case List("T_USE", "use_declarations", "T_SEMICOLON") => notyet(n)
            case List("constant_declaration", "T_SEMICOLON") => notyet(n)
            case _ => unspecified(n)
        }
    }

    def statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") => inner_statement_list(child(n,1))
            case List("T_IF", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "statement", "elseif_list", "else_single") =>
                If(expr(child(n, 2)), statement(child(n, 4)), elseif_else(elseif_list(child(n, 5)), else_single(child(n, 6))))
            case List("T_IF", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "T_COLON", "inner_statement_list", "new_elseif_list", "new_else_single", "T_ENDIF", "T_SEMICOLON") =>
                If(expr(child(n, 2)), inner_statement_list(child(n, 5)), elseif_else(elseif_list(child(n, 6)), else_single(child(n, 7))))
            case List("T_WHILE", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "while_statement") =>
                While(expr(child(n, 2)), while_statement(child(n, 4)))
            case List("T_DO", "statement", "T_WHILE", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "T_SEMICOLON") =>
                DoWhile(statement(child(n, 1)), expr(child(n, 4)))
            case List("T_FOR", "T_OPEN_BRACES", "for_expr", "T_SEMICOLON", "for_expr", "T_SEMICOLON", "for_expr", "T_CLOSE_BRACES", "for_statement") =>
                For(for_expr(child(n, 2)), for_expr(child(n, 4)), for_expr(child(n, 6)), for_statement(child(n, 8)))
            case List("T_SWITCH", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "switch_case_list") => 
                Switch(expr(child(n, 2)), switch_case_list(child(n, 4)))
            case List("T_BREAK", "T_SEMICOLON") =>
                Break(PHPInteger(1))
            case List("T_BREAK", "expr", "T_SEMICOLON") =>
                Break(expr(child(n, 1)))
            case List("T_CONTINUE", "T_SEMICOLON") =>
                Continue(PHPInteger(1))
            case List("T_CONTINUE", "expr", "T_SEMICOLON") =>
                Continue(expr(child(n, 1)))
            case List("T_RETURN", "T_SEMICOLON") =>
                Return(Null())
            case List("T_RETURN", "expr", "T_SEMICOLON") =>
                Return(expr(child(n, 1)))
            case List("T_GLOBAL", "global_var_list", "T_SEMICOLON") => 
                Global(global_var_list(child(n, 1)))
            case List("T_STATIC", "static_var_list", "T_SEMICOLON") => 
                Static(static_var_list(child(n, 1)))
            case List("T_ECHO", "echo_expr_list", "T_SEMICOLON") => 
                Echo(echo_expr_list(child(n, 1)))
            case List("T_INLINE_HTML") =>
                Html(child(n, 0).tokenContent)
            case List("expr", "T_SEMICOLON") =>
                expr(child(n, 0))
            case List("T_UNSET", "T_OPEN_BRACES", "variable_list", "T_CLOSE_BRACES", "T_SEMICOLON") =>
                Unset(variable_list(child(n, 2)))
            case List("T_FOREACH", "T_OPEN_BRACES", "expr", "T_AS", "foreach_variable", "foreach_optional_arg", "T_CLOSE_BRACES", "foreach_statement") =>
                foreach_variable(child(n, 4)) match {
                    case (v, byref) => foreach_optional_arg(child(n, 5)) match {
                            case Some((v2, byref2)) =>
                                Foreach(expr(child(n, 2)), v2, byref2, Some(v), byref, foreach_statement(child(n, 7)))
                            case None => 
                                Foreach(expr(child(n, 2)), v, byref, None, false, foreach_statement(child(n, 7)))
                        }
                }
            case List("T_DECLARE", "T_OPEN_BRACES", "declare_list", "T_CLOSE_BRACES", "declare_statement") =>
                Void() /* ignored */
            case List("T_SEMICOLON") => 
                Void()
            case List("T_TRY", "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES", "T_CATCH", "T_OPEN_BRACES", "fully_qualified_class_name", "T_VARIABLE", "T_CLOSE_BRACES", "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES", "additional_catches") =>
                Try(inner_statement_list(child(n, 2)),
                    List(Catch(fully_qualified_class_name(child(n, 6)),
                              SimpleVariable(Identifier(child(n, 7).tokenContent)),
                              inner_statement_list(child(n, 10)))) 
                    ::: additional_catches(child(n, 12))
                )
            case List("T_THROW", "expr", "T_SEMICOLON") =>
                Throw(expr(child(n, 1)))
            case List("T_GOTO", "T_STRING", "T_SEMICOLON") =>
                Goto(Label(Identifier(child(n, 1).tokenContent)))
            case List("T_STRING", "T_COLON") =>
                LabelDecl(Identifier(child(n,0).tokenContent))
        }
    }

    def static_var_list(n: ParseNode): List[InitVariable] = {
        childrenNames(n) match {
            case List("static_var_list", "T_COMMA", "T_VARIABLE") =>
                static_var_list(child(n, 0)) ::: List(InitVariable(t_variable(child(n, 2)), None))
            case List("static_var_list", "T_COMMA", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                static_var_list(child(n, 0)) ::: List(InitVariable(t_variable(child(n, 2)), Some(static_expr(child(n, 4)))))
            case List("T_VARIABLE") =>
                List(InitVariable(t_variable(child(n, 0)), None))
            case List("T_VARIABLE", "T_ASSIGN", "static_expr") =>
                List(InitVariable(t_variable(child(n, 0)), Some(static_expr(child(n, 2)))))
        }
    }

    def static_expr(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("common_scalar") =>
                common_scalar(child(n))
            case List("namespace_name") =>
                unspecified(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") =>
                unspecified(n)
            case List("T_NS_SEPARATOR", "namespace_name") =>
                unspecified(n)
            case List("T_PLUS", "static_expr") =>
                static_expr(child(n, 1))
            case List("T_MINUS", "static_expr") =>
                Minus(PHPInteger(0), static_expr(child(n, 1)))
            case List("T_ARRAY", "T_OPEN_BRACES", "static_array_pair_list", "T_CLOSE_BRACES") =>
                Array(static_array_pair_list(child(n, 2)))
            case List("static_class_constant") =>
                static_class_constant(child(n))
        }
    }

    def common_scalar(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_LNUMBER") =>
                PHPInteger(child(n).tokenContent.toInt)
            case List("T_DNUMBER") =>
                PHPFloat(child(n).tokenContent.toFloat)
            case List("T_CONSTANT_ENCAPSED_STRING") =>
                PHPString(child(n).tokenContent)
            case List("T_LINE") =>
                MCLine()
            case List("T_FILE") =>
                MCFile()
            case List("T_DIR") =>
                MCDir()
            case List("T_CLASS_C") =>
                MCClass()
            case List("T_METHOD_C") =>
                MCMethod()
            case List("T_FUNC_C") =>
                MCFunction()
            case List("T_NS_C") =>
                MCNamespace()
            case List("T_START_HEREDOC", "T_ENCAPSED_AND_WHITESPACE", "T_END_HEREDOC") =>
                PHPString(child(n).tokenContent)
            case List("T_START_HEREDOC", "T_END_HEREDOC") =>
                PHPString("")
        }
    }

    def static_class_constant(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("class_name", "T_PAAMAYIM_NEKUDOTAYIM", "T_STRING") =>
                ClassConstant(class_name(child(n, 0)), Identifier(child(n, 2).tokenContent))
        }
    }

    def class_name(n: ParseNode): ClassRef = {
        childrenNames(n) match {
            case List("T_STATIC") => CalledClass()
            case List("namespace_name") => fully_qualified_class_name(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") => fully_qualified_class_name(n)
            case List("T_NS_SEPARATOR", "namespace_name") => fully_qualified_class_name(n)
        }
    }

    def static_array_pair_list(n: ParseNode): List[(Option[Expression], Expression)] = {
        childrenNames(n) match {
            case List() => List()
            case List("non_empty_static_array_pair_list", "possible_comma") => non_empty_static_array_pair_list(child(n, 0))
        }
    }

    def non_empty_static_array_pair_list(n: ParseNode): List[(Option[Expression], Expression)] = {
        childrenNames(n) match {
            case List("non_empty_static_array_pair_list", "T_COMMA", "static_expr", "T_DOUBLE_ARROW", "static_expr") =>
                non_empty_static_array_pair_list(child(n, 0)) ::: List((Some(static_expr(child(n, 2))), static_expr(child(n, 4))))
            case List("non_empty_static_array_pair_list", "T_COMMA", "static_expr") =>
                non_empty_static_array_pair_list(child(n, 0)) ::: List((None, static_expr(child(n, 2))))
            case List("static_expr", "T_DOUBLE_ARROW", "static_expr") =>
                List((Some(static_expr(child(n, 0))), static_expr(child(n, 2))))
            case List("static_expr") =>
                List((None, static_expr(child(n, 0))))
        }
    }

    def global_var_list(n: ParseNode): List[Variable] = {
        childrenNames(n) match {
            case List("global_var") => 
                List(global_var(child(n)))
            case List("global_var_list", "T_COMMA", "global_var") =>
                global_var_list(child(n, 0)) ::: List(global_var(child(n , 2)))
        }
    }

    def global_var(n: ParseNode): Variable = {
        childrenNames(n) match {
            case List("T_VARIABLE") => 
                t_variable(child(n))
            case List("T_DOLLAR", "variable") => 
                VariableVariable(variable(child(n, 1)))
            case List("T_DOLLAR", "T_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                VariableVariable(expr(child(n, 1)))
        }
    }

    def additional_catches(n: ParseNode): List[Catch] = {
        childrenNames(n) match {
            case List() => List()
            case List("non_empty_additional_catches") => non_empty_additional_catches(child(n))
        }
    }

    def non_empty_additional_catches(n: ParseNode): List[Catch] = {
        childrenNames(n) match {
            case List("additional_catch") =>
                List(additional_catch(child(n)))
            case List("non_empty_additional_catches", "additional_catch") =>
                non_empty_additional_catches(child(n, 0)) ::: List(additional_catch(child(n,1)))
        }
    }

    def additional_catch(n: ParseNode): Catch = {
        childrenNames(n) match {
            case List("T_CATCH", "T_OPEN_BRACES", "fully_qualified_class_name", "T_VARIABLE", "T_CLOSE_BRACES",
                      "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") =>
                Catch(fully_qualified_class_name(child(n, 2)),
                      t_variable(child(n, 3)),
                      inner_statement_list(child(n, 6)))
        }
    }

    def inner_statement_list(n: ParseNode): Block = {
        def inner_statement_list2(n: ParseNode): List[Statement] = {
            childrenNames(n) match {
                case List("inner_statement_list", "inner_statement") => 
                    inner_statement_list2(child(n, 0)) ::: List(inner_statement(child(n,1)))
                case List() =>
                    List()
            }
        }
        Block(inner_statement_list2(n))
    }

    def inner_statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("statement") => statement(child(n))
            case List("function_declaration_statement") => function_declaration_statement(child(n))
            case List("class_declaration_statement") => class_declaration_statement(child(n))
            case List("T_HALT_COMPILER", "T_OPEN_BRACES", "T_CLOSE_BRACES", "T_SEMICOLON") => notyet(n)
        }
    }

    def foreach_variable(n: ParseNode): (Variable, Boolean) = {
        childrenNames(n) match {
            case List("variable") => (variable(child(n)), false)
            case List("T_BITWISE_AND", "variable") => (variable(child(n, 1)), true)
        }
    }

    def foreach_statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("statement") => statement(child(n))
            case List("T_COLON", "inner_statement_list", "T_ENDFOREACH", "T_SEMICOLON") => statement(child(n, 1))
        }
    }

    def foreach_optional_arg(n: ParseNode): Option[(Variable, Boolean)] = {
        childrenNames(n) match {
            case List() => None
            case List("T_DOUBLE_ARROW", "foreach_variable") => Some(foreach_variable(child(n, 1)))
        }
    }

    def namespace_name(n: ParseNode): List[Identifier] = {
        childrenNames(n) match {
            case List("T_STRING") => 
                List(Identifier(child(n).tokenContent))
            case List("namespace_name", "T_NS_SEPARATOR", "T_STRING") =>
                namespace_name(child(n, 0)) ::: List(Identifier(child(n, 2).tokenContent))
        }
    }

    def fully_qualified_class_name(n: ParseNode): ClassRef = {
        var root: NSRoot = NSNone
        var c: ParseNode = childrenNames(n) match {
            case List("namespace_name") => root = NSNone; child(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") => root = NSCurrent; child(n, 2)
            case List("T_NS_SEPARATOR", "namespace_name") => root = NSGlobal; child(n, 1)
        }
        val parts = namespace_name(c);
        StaticClassRef(root, parts.init, parts.last)
    }

    def variable_list(n: ParseNode): List[Variable] = {
        childrenNames(n) match {
            case List("variable") => 
                List(variable(child(n)))
            case List("variable_list", "T_COMMA", "variable") =>
                variable_list(child(n, 0)) ::: List(variable(child(n, 2)))
        }
    }

    def switch_case_list(n: ParseNode): List[(Option[Expression], Statement)] = {
        childrenNames(n) match {
            case List("T_OPEN_CURLY_BRACES", "case_list", "T_CLOSE_CURLY_BRACES") => case_list(child(n, 1))
            case List("T_OPEN_CURLY_BRACES", "T_SEMICOLON", "case_list", "T_CLOSE_CURLY_BRACES") => case_list(child(n, 2))
            case List("T_COLON", "case_list", "T_ENDSWITCH", "T_SEMICOLON") => case_list(child(n, 1))
            case List("T_COLON", "T_SEMICOLON", "case_list", "T_ENDSWITCH", "T_SEMICOLON") => case_list(child(n, 2))
        }
    }

    def case_list(n: ParseNode): List[(Option[Expression], Statement)] = {
        childrenNames(n) match {
            case List() => List()
            case List("case_list", "T_CASE", "expr", "case_separator", "inner_statement_list") => 
                case_list(child(n, 0)) ::: (Some(expr(child(n, 2))), inner_statement_list(child(n, 4))) :: Nil
            case List("case_list", "T_DEFAULT", "case_separator", "inner_statement_list") =>
                case_list(child(n, 0)) ::: (None, inner_statement_list(child(n, 3))) :: Nil
        }
    }

    def for_expr(n: ParseNode): List[Expression] = {
        childrenNames(n) match {
            case List() => List()
            case List("non_empty_for_expr") => non_empty_for_expr(child(n))
        }
    }

    def non_empty_for_expr(n: ParseNode): List[Expression] = {
        childrenNames(n) match {
            case List("non_empty_for_expr", "T_COMMA", "expr") => non_empty_for_expr(child(n, 0)) ::: List(expr(child(n, 2)))
            case List("expr") => List(expr(child(n)))
        }
    }

    def for_statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("statement") => statement(child(n))
            case List("T_COLON", "inner_statement_list", "T_ENDFOR", "T_SEMICOLON") => inner_statement_list(child(n, 1))
        }
    }

    def while_statement(n: ParseNode): Statement = {
        childrenNames(n) match {
            case List("statement") => statement(child(n))
            case List("T_COLON", "inner_statement_list", "T_ENDWHILE", "T_SEMICOLON") => inner_statement_list(child(n, 1))
        }
    }

    def elseif_list(n: ParseNode): List[(Expression, Statement)] = {
        childrenNames(n) match {
            case List() => List()
            case List("elseif_list", "T_ELSEIF", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "statement") =>
                elseif_list(child(n, 0)) ::: List((expr(child(n, 3)), statement(child(n, 5))))
            case List("new_elseif_list", "T_ELSEIF", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "T_COLON", "inner_statement_list") =>
                elseif_list(child(n, 0)) ::: List((expr(child(n, 3)), inner_statement_list(child(n, 6))))
        }
    }

    def else_single(n: ParseNode): Option[Statement] = {
        childrenNames(n) match {
            case List() => None
            case List("T_ELSE", "statement") => Some(statement(child(n, 1)))
            case List("T_ELSE", "T_COLON", "inner_statement_list") => Some(inner_statement_list(child(n, 2)))
        }
    }

    def elseif_else(elseifs: List[(Expression, Statement)], elze: Option[Statement]): Option[Statement] = {
        elseifs match {
            case (expr, stat) :: rest => Some(If(expr, stat, elseif_else(rest, elze)))
            case Nil => elze
        }
    }

    def function_declaration_statement(n: ParseNode): FunctionDecl = {
        notyet(n);
    }

    def class_declaration_statement(n: ParseNode): ClassDecl = {
        notyet(n);
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
                notyet(n)
            case List("T_PRINT", "expr") =>
                Print(expr(child(n, 1)))
            case List("T_FUNCTION", "is_reference", "T_OPEN_BRACES", "parameter_list", "T_CLOSE_BRACES", "lexical_vars", "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") =>
                notyet(n)
            case _ => unspecified(n)
        }
    }

    def variable(n: ParseNode): Variable = {
        childrenNames(n) match {
            case List("base_variable_with_function_calls", "T_OBJECT_OPERATOR", "object_property", "method_or_not", "variable_properties") =>
            case List("base_variable_with_function_calls") =>
        }
    }

    def reference_variable(n: ParseNode): Variable = {
        notyet(n)
    }


    def scalar(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_STRING_VARNAME") =>
                unspecified(n)
            case List("class_constant") =>
                class_constant(child(n))
            case List("namespace_name") =>
                notyet(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") =>
                notyet(n)
            case List("T_NS_SEPARATOR", "namespace_name") =>
                notyet(n)
            case List("common_scalar") =>
                common_scalar(child(n))
            case List("T_DOUBLE_QUOTE", "encaps_list", "T_DOUBLE_QUOTE") => 
                encaps_list(child(n, 1))
            case List("T_START_HEREDOC", "encaps_list", "T_END_HEREDOC") =>
                encaps_list(child(n, 1))
        }
    }

    def class_constant(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("class_name", "T_PAAMAYIM_NEKUDOTAYIM T_STRING") =>
                ClassConstant(class_name(child(n, 0)), Identifier(child(n, 2).tokenContent))
            case List("reference_variable", "T_PAAMAYIM_NEKUDOTAYIM T_STRING") =>
                ClassConstant(VarClassRef(reference_variable(child(n, 0))), Identifier(child(n, 2).tokenContent))
        }
    }

    def encaps_list(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("encaps_list", "encaps_var") =>
                Concat(encaps_list(child(n, 0)), encaps_var(child(n, 1)))
            case List("encaps_list", "T_ENCAPSED_AND_WHITESPACE") =>
                Concat(encaps_list(child(n, 0)), PHPString(child(n, 1).tokenContent))
            case List("encaps_var") =>
                encaps_var(child(n, 1))
            case List("T_ENCAPSED_AND_WHITESPACE", "encaps_var") =>
                PHPString(child(n, 1).tokenContent)
        }
    }

    def encaps_var(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_VARIABLE") =>
                t_variable(child(n))
            case List("T_VARIABLE", "T_OPEN_RECT_BRACES", "encaps_var_offset", "T_CLOSE_RECT_BRACES") =>
                ArrayEntry(t_variable(child(n, 0)), encaps_var_offset(child(n, 2)))
            case List("T_VARIABLE", "T_OBJECT_OPERATOR", "T_STRING") =>
                ObjectProperty(t_variable(child(n, 0)), Identifier(child(n, 2).tokenContent))
            case List("T_DOLLAR_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                VariableVariable(expr(child(n, 1)))
            case List("T_DOLLAR_OPEN_CURLY_BRACES", "T_STRING_VARNAME", "T_OPEN_RECT_BRACES", "expr", "T_CLOSE_RECT_BRACES", "T_CLOSE_CURLY_BRACES") =>
                ArrayEntry(SimpleVariable(Identifier(child(n, 1).tokenContent)), expr(child(n, 3)))
            case List("T_CURLY_OPEN", "variable", "T_CLOSE_CURLY_BRACES") =>
                variable(child(n, 1))
        }
    }

    def t_variable(n: ParseNode): SimpleVariable = SimpleVariable(Identifier(n.tokenContent))

    def encaps_var_offset(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_STRING") => PHPString(child(n).tokenContent)
            case List("T_NUM_STRING") => PHPString(child(n).tokenContent)
            case List("T_VARIABLE") => t_variable(child(n))
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
