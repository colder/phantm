// This class transforms a Syntax Tree to an Abstract Syntax Tree
package phpanalysis.parser;

import phpanalysis.parser.Trees._;

case class STToAST(comp: Compiler, st: ParseNode) {

    def getAST = S(st);

    // Tree visitors functions

    def S(n: ParseNode): Program = new Program(top_statement_list(child(n, 0))).setPos(n)

    def top_statement_list(n: ParseNode): List[Statement] = {
        childrenNames(n) match {
            case List() => Nil
            case List("top_statement_list", "top_statement") => 
                top_statement_list(child(n, 0)) ::: List(top_statement(child(n, 1)))
        }
    }

    def top_statement(n: ParseNode): Statement = {
        (childrenNames(n) match {
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
        }).setPos(child(n))
    }
    def class_declaration_statement(n: ParseNode): Statement = {
        (childrenNames(n) match {
            case List("class_entry_type", "T_STRING", "extends_from", "implements_list", "T_OPEN_CURLY_BRACES", "class_statement_list", "T_CLOSE_CURLY_BRACES") =>
                class_statement_list(child(n, 5)) match {
                    case (methods, static_props, props, consts) =>
                        ClassDecl(identifier(child(n, 1)),
                                  class_entry_type(child(n, 0)),
                                  extends_from(child(n, 2)),
                                  implements_list(child(n, 3)),
                                  methods,
                                  static_props,
                                  props,
                                  consts)
                }

            case List("interface_entry", "T_STRING", "interface_extends_list", "T_OPEN_CURLY_BRACES", "class_statement_list", "T_CLOSE_CURLY_BRACES") =>
                class_statement_list(child(n, 4)) match {
                    case (methods, static_props, props, consts) =>
                        InterfaceDecl(identifier(child(n, 1)),
                                  interface_extends_list(child(n, 2)),
                                  methods,
                                  consts)
                }
        }).setPos(child(n))
    }

    def class_statement_list(n: ParseNode): (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ConstantDecl]) = {
        childrenNames(n) match {
            case List("class_statement_list", "class_statement") =>
                class_statement(child(n, 1), class_statement_list(child(n, 0)));
            case List() =>
                (List(), List(), List(), List())
        }
    }

    def class_statement(n: ParseNode, st: (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ConstantDecl])): (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ConstantDecl]) = { 
        childrenNames(n) match {
            case List("variable_modifiers", "class_variable_declaration", "T_SEMICOLON") =>
                val vm = variable_modifiers(child(n, 0))
                var pd = class_variable_declaration(child(n, 1), vm)

                if (vm exists { _ == MFStatic }) {
                    (st._1, st._2:::pd, st._3, st._4)
                } else {
                    (st._1, st._2, st._3:::pd, st._4)
                }
            case List("class_constant_declaration", "T_SEMICOLON") =>
                val cd = class_constant_declaration(child(n, 0));
                comp.clearPreviousComment(cd.last)
                (st._1, st._2, st._3, st._4:::cd)
            case List("method_modifiers", "T_FUNCTION", "is_reference", "T_STRING", "T_OPEN_BRACES", "parameter_list", "T_CLOSE_BRACES", "method_body") =>
                val pos = new Position().setPos(child(n,3));
                val com = comp.getPreviousComment(pos);

                var md = MethodDecl(identifier(child(n, 3)),
                                    method_modifiers(child(n, 0)),
                                    parameter_list(child(n, 5)),
                                    is_reference(child(n, 2)),
                                    None,
                                    method_body(child(n, 7))).setPos(pos).attachComment(com);

                (st._1:::List(md), st._2, st._3, st._4)
        }
    }

    def parameter_list(n: ParseNode): List[ArgumentDecl] = {
        childrenNames(n) match {
            case List("non_empty_parameter_list") =>
                non_empty_parameter_list(child(n))
            case List() =>
                List()
        }
    }

    def non_empty_parameter_list(n: ParseNode): List[ArgumentDecl] = {
        childrenNames(n) match {
            case List("optional_class_type", "T_VARIABLE") =>
                List(ArgumentDecl(t_variable(child(n, 1)), optional_class_type(child(n, 0)), None, false).setPos(child(n, 1)))
            case List("optional_class_type", "T_BITWISE_AND", "T_VARIABLE") =>
                List(ArgumentDecl(t_variable(child(n, 2)), optional_class_type(child(n, 0)), None, true).setPos(child(n, 2)))
            case List("optional_class_type", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                List(ArgumentDecl(t_variable(child(n, 1)), optional_class_type(child(n, 0)), Some(static_expr(child(n, 3))), false).setPos(child(n, 1)))
            case List("optional_class_type", "T_BITWISE_AND", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                List(ArgumentDecl(t_variable(child(n, 2)), optional_class_type(child(n, 0)), Some(static_expr(child(n, 4))), true).setPos(child(n, 2)))
            case List("non_empty_parameter_list", "T_COMMA", "optional_class_type", "T_VARIABLE") =>
                non_empty_parameter_list(child(n, 0)) :::
                List(ArgumentDecl(t_variable(child(n, 3)), optional_class_type(child(n, 2)), None, false).setPos(child(n, 3)))
            case List("non_empty_parameter_list", "T_COMMA", "optional_class_type", "T_BITWISE_AND", "T_VARIABLE") =>
                non_empty_parameter_list(child(n, 0)) :::
                List(ArgumentDecl(t_variable(child(n, 4)), optional_class_type(child(n, 2)), None, true).setPos(child(n, 4)))
            case List("non_empty_parameter_list", "T_COMMA", "optional_class_type", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                non_empty_parameter_list(child(n, 0)) :::
                List(ArgumentDecl(t_variable(child(n, 3)), optional_class_type(child(n, 2)), Some(static_expr(child(n, 5))), false).setPos(child(n, 3)))
            case List("non_empty_parameter_list", "T_COMMA", "optional_class_type", "T_BITWISE_AND", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                non_empty_parameter_list(child(n, 0)) :::
                List(ArgumentDecl(t_variable(child(n, 4)), optional_class_type(child(n, 2)), Some(static_expr(child(n, 6))), true).setPos(child(n, 4)))
        }
    }

    def is_reference(n: ParseNode): Boolean = {
        childrenNames(n) match {
            case List() => false
            case List("T_BITWISE_AND") => true
        }
    }

    def method_body(n: ParseNode): Option[Statement] = {
        childrenNames(n) match {
            case List("T_SEMICOLON") => 
                None
            case List("T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") =>
                Some(inner_statement_list(child(n, 1)))
        }
    }

    def optional_class_type(n: ParseNode): Option[TypeHint] = {
        childrenNames(n) match {
            case List() => None 
            case List("fully_qualified_class_name") =>
                val cn = fully_qualified_class_name(child(n))
                Some(THObject(cn).setPos(cn))
            case List("T_ARRAY") => 
                Some(THArray.setPos(child(n)))
        }
    }

    def class_variable_declaration(n: ParseNode, vm: List[MemberFlag]): List[PropertyDecl] = {
        childrenNames(n) match {
            case List("class_variable_declaration", "T_COMMA", "T_VARIABLE") =>
                val pos = new Position().setPos(child(n, 2))
                val com = comp.getPreviousComment(pos);
                val pd = PropertyDecl(varIdentifier(child(n, 2)), vm, None, None).setPos(pos).attachComment(com)

                class_variable_declaration(child(n, 0), vm) ::: List(pd)

            case List("class_variable_declaration", "T_COMMA", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                val pos = new Position().setPos(child(n, 2))
                val com = comp.getPreviousComment(pos);
                val pd = PropertyDecl(varIdentifier(child(n, 2)), vm, Some(static_expr(child(n, 4))), None).setPos(pos).attachComment(com)

                class_variable_declaration(child(n, 0), vm) ::: List(pd)

            case List("T_VARIABLE") =>
                val pos = new Position().setPos(child(n, 0))
                val com = comp.getPreviousComment(pos);
                val pd = PropertyDecl(varIdentifier(child(n, 0)), vm, None, None).setPos(pos).attachComment(com)

                List(pd)

            case List("T_VARIABLE", "T_ASSIGN", "static_expr") =>
                val pos = new Position().setPos(child(n, 0))
                val com = comp.getPreviousComment(pos);
                val pd = PropertyDecl(varIdentifier(child(n, 0)), vm, Some(static_expr(child(n, 2))), None).setPos(pos).attachComment(com)

                List(pd)
        }
    }

    def class_constant_declaration(n: ParseNode): List[ConstantDecl] = {
        childrenNames(n) match {
            case List("class_constant_declaration", "T_COMMA", "T_STRING", "T_ASSIGN", "static_expr") =>
                val pos = new Position().setPos(child(n, 2))
                val com = comp.getPreviousComment(pos);
                class_constant_declaration(child(n, 0)) ::: List(ConstantDecl(identifier(child(n, 2)), static_expr(child(n, 4))).setPos(child(n, 2)).attachComment(com))
            case List("T_CONST", "T_STRING", "T_ASSIGN", "static_expr") =>
                val pos = new Position().setPos(child(n, 1))
                val com = comp.getPreviousComment(pos);
                List(ConstantDecl(identifier(child(n, 1)), static_expr(child(n, 3))).setPos(child(n, 1)).attachComment(com))
        }
    }

    def variable_modifiers(n: ParseNode): List[MemberFlag] = {
        childrenNames(n) match {
            case List("non_empty_member_modifiers") =>
                non_empty_member_modifiers(child(n))
            case List("T_VAR") =>
                List(MFPublic.setPos(child(n, 0)))
        }
    }

    def method_modifiers(n: ParseNode): List[MemberFlag] = {
        childrenNames(n) match {
            case List("non_empty_member_modifiers") =>
                non_empty_member_modifiers(child(n))
            case List() =>
                List()
        }
    }

    def non_empty_member_modifiers(n: ParseNode): List[MemberFlag] = {
        childrenNames(n) match {
            case List("member_modifier") =>
                List(member_modifier(child(n, 0)))
            case List("non_empty_member_modifiers", "member_modifier") =>
                non_empty_member_modifiers(child(n, 0)):::List(member_modifier(child(n, 1)))
        }
    }

    def member_modifier(n: ParseNode): MemberFlag = {
        (childrenNames(n) match {
            case List("T_PUBLIC") => MFPublic
            case List("T_PROTECTED") => MFProtected
            case List("T_PRIVATE") => MFPrivate
            case List("T_STATIC") => MFStatic
            case List("T_ABSTRACT") => MFAbstract
            case List("T_FINAL") => MFFinal
        }).setPos(child(n, 0));
    }

    def class_entry_type(n: ParseNode): ClassFlag = {
        (childrenNames(n) match {
            case List("T_CLASS") => CFNormal
            case List("T_ABSTRACT", "T_CLASS") => CFAbstract
            case List("T_FINAL", "T_CLASS") => CFFinal
        }).setPos(child(n, 0))
    }

    def extends_from(n: ParseNode): Option[StaticClassRef] = {
        childrenNames(n) match {
            case List() =>
                None
            case List("T_EXTENDS", "fully_qualified_class_name") =>
                Some(fully_qualified_class_name(child(n, 1)))
        }
    }

    def interface_extends_list(n: ParseNode): List[ClassRef] = {
        childrenNames(n) match {
            case List() =>
                List()
            case List("T_EXTENDS", "interface_list") =>
                interface_list(child(n, 1))
        }
    }

    def implements_list(n: ParseNode): List[StaticClassRef] = {
        childrenNames(n) match {
            case List() =>
                List()
            case List("T_IMPLEMENTS", "interface_list") =>
                interface_list(child(n, 1))
        }
    }

    def interface_list(n: ParseNode): List[StaticClassRef] = {
        childrenNames(n) match {
            case List("fully_qualified_class_name") =>
                List(fully_qualified_class_name(child(n)))
            case List("interface_list", "T_COMMA", "fully_qualified_class_name") =>
                interface_list(child(n, 0)) ::: List(fully_qualified_class_name(child(n, 2)))
        }
    }

    def statement(n: ParseNode): Statement = {
        val pos = new Position().setPos(child(n, 0))
        val com = comp.getPreviousComment(pos);

        (childrenNames(n) match {
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
                val conds = for_expr(child(n, 4))

                For(Block(for_expr(child(n, 2))), conds reduceLeft { (x, y) => BooleanAnd(x,y) }, Block(for_expr(child(n, 6))), for_statement(child(n, 8)))
            case List("T_SWITCH", "T_OPEN_BRACES", "expr", "T_CLOSE_BRACES", "switch_case_list") => 
                Switch(expr(child(n, 2)), switch_case_list(child(n, 4)))
            case List("T_BREAK", "T_SEMICOLON") =>
                Break(PHPInteger(1).setPos(child(n, 0)))
            case List("T_BREAK", "expr", "T_SEMICOLON") =>
                Break(expr(child(n, 1)))
            case List("T_CONTINUE", "T_SEMICOLON") =>
                Continue(PHPInteger(1).setPos(child(n, 0)))
            case List("T_CONTINUE", "expr", "T_SEMICOLON") =>
                Continue(expr(child(n, 1)))
            case List("T_RETURN", "T_SEMICOLON") =>
                Return(PHPNull().setPos(child(n, 0)))
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
                              t_variable(child(n, 7)),
                              inner_statement_list(child(n, 10))).setPos(child(n, 4))) 
                    ::: additional_catches(child(n, 12))
                )
            case List("T_THROW", "expr", "T_SEMICOLON") =>
                Throw(expr(child(n, 1)))
            case List("T_GOTO", "T_STRING", "T_SEMICOLON") =>
                Goto(Label(identifier(child(n, 1))).setPos(child(n, 1)))
            case List("T_STRING", "T_COLON") =>
                LabelDecl(Identifier(child(n,0).tokenContent).setPos(child(n, 0)))
        }).setPos(child(n)).attachComment(com)
    }

    def static_var_list(n: ParseNode): List[InitVariable] = {
        childrenNames(n) match {
            case List("static_var_list", "T_COMMA", "T_VARIABLE") =>
                val list = static_var_list(child(n, 0))

                val pos = new Position().setPos(child(n, 2))
                val com = comp.getPreviousComment(pos);

                 list ::: List(InitVariable(t_variable(child(n, 2)), None).setPos(child(n, 2)).attachComment(com))
            case List("static_var_list", "T_COMMA", "T_VARIABLE", "T_ASSIGN", "static_expr") =>
                val list = static_var_list(child(n, 0))

                val pos = new Position().setPos(child(n, 2))
                val com = comp.getPreviousComment(pos);

                list ::: List(InitVariable(t_variable(child(n, 2)), Some(static_expr(child(n, 4)))).setPos(child(n, 2)).attachComment(com))
            case List("T_VARIABLE") =>
                val pos = new Position().setPos(child(n, 0))
                val com = comp.getPreviousComment(pos);

                List(InitVariable(t_variable(child(n, 0)), None).setPos(child(n, 0)).attachComment(com))
            case List("T_VARIABLE", "T_ASSIGN", "static_expr") =>
                val pos = new Position().setPos(child(n, 0))
                val com = comp.getPreviousComment(pos);

                List(InitVariable(t_variable(child(n, 0)), Some(static_expr(child(n, 2)))).setPos(child(n, 0)).attachComment(com))
        }
    }

    def static_constant(n: ParseNode): Expression = {
        var root: NSRoot = NSNone
        var c: ParseNode = childrenNames(n) match {
            case List("namespace_name") => root = NSNone; child(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") => root = NSCurrent; child(n, 2)
            case List("T_NS_SEPARATOR", "namespace_name") => root = NSGlobal; child(n, 1)
        }
        val parts = namespace_name(c);
        root.setPos(child(n, 0))

        if (parts.length == 1) {
            var res : Expression = Constant(parts.head);
            if (parts.head.value.toLowerCase.equals("true")){
                res = PHPTrue()
            } else if (parts.head.value.toLowerCase.equals("false")) {
                res = PHPFalse()
            } else if (parts.head.value.toLowerCase.equals("null")) {
                res = PHPNull()
            }
            res.setPos(parts.head)
        } else {
            unspecified(n)
        }
    }
    def static_expr(n: ParseNode): Expression = {
        val pos = new Position().setPos(child(n, 0))
        val com = comp.getPreviousComment(pos);

        (childrenNames(n) match {
            case List("common_scalar") =>
                common_scalar(child(n))
            case List("namespace_name") =>
                static_constant(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") =>
                static_constant(n)
            case List("T_NS_SEPARATOR", "namespace_name") =>
                static_constant(n)
            case List("T_PLUS", "static_expr") =>
                static_expr(child(n, 1))
            case List("T_MINUS", "static_expr") =>
                Minus(PHPInteger(0).setPos(child(n, 0)), static_expr(child(n, 1)))
            case List("T_ARRAY", "T_OPEN_BRACES", "static_array_pair_list", "T_CLOSE_BRACES") =>
                Array(static_array_pair_list(child(n, 2)))
            case List("static_class_constant") =>
                static_class_constant(child(n))
        }).setPos(pos).attachComment(com)
    }

    def common_scalar(n: ParseNode): Expression = {
        val pos = new Position().setPos(child(n, 0))
        val com = comp.getPreviousComment(pos);

        (childrenNames(n) match {
            case List("T_LNUMBER") =>
                // dispatch based on the type
                val str = child(n).tokenContent;

                try {
                    val l = if (str.startsWith("0x") || str.startsWith("0X")) {
                        java.lang.Long.parseLong(str.substring(2), 16)
                    } else if (str.startsWith("0") && str.length() > 1) {
                        java.lang.Long.parseLong(str.substring(1), 8)
                    } else {
                        java.lang.Long.parseLong(str, 10)
                    }

                    PHPInteger(l)
                } catch {
                    case e: java.lang.NumberFormatException =>
                        Reporter.notice("Number format error in '"+str+"': "+e.getMessage, pos)
                        PHPInteger(0)
                }
            case List("T_DNUMBER") =>
                val str = child(n).tokenContent
                try {
                    PHPFloat(str.toFloat)
                } catch {
                    case e: java.lang.NumberFormatException =>
                        Reporter.notice("Number format error in '"+str+"': "+e.getMessage, pos)
                        PHPFloat(0)
                }
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
                PHPString(child(n, 1).tokenContent)
            case List("T_START_HEREDOC", "T_CONSTANT_ENCAPSED_STRING", "T_END_HEREDOC") =>
                PHPString(child(n, 1).tokenContent)
            case List("T_START_HEREDOC", "T_END_HEREDOC") =>
                PHPString("")
        }).setPos(pos).attachComment(com)
    }

    def static_class_constant(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("class_name", "T_DOUBLE_COLON", "T_STRING") =>
                val pos = new Position().setPos(child(n, 0))
                val com = comp.getPreviousComment(pos);

                val cn = class_name(child(n, 0))
                ClassConstant(cn, identifier(child(n, 2))).setPos(cn).attachComment(com)
        }
    }

    def class_name(n: ParseNode): ClassRef = {
        (childrenNames(n) match {
            case List("T_STATIC") => CalledClass()
            case List("namespace_name") => fully_qualified_class_name(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") => fully_qualified_class_name(n)
            case List("T_NS_SEPARATOR", "namespace_name") => fully_qualified_class_name(n)
        }).setPos(child(n))
    }

    def static_array_pair_list(n: ParseNode): List[(Option[Expression], Expression, Boolean)] = {
        childrenNames(n) match {
            case List() => List()
            case List("non_empty_static_array_pair_list", "possible_comma") => non_empty_static_array_pair_list(child(n, 0))
        }
    }

    def non_empty_static_array_pair_list(n: ParseNode): List[(Option[Expression], Expression, Boolean)] = {
        childrenNames(n) match {
            case List("non_empty_static_array_pair_list", "T_COMMA", "static_expr", "T_DOUBLE_ARROW", "static_expr") =>
                non_empty_static_array_pair_list(child(n, 0)) ::: List((Some(static_expr(child(n, 2))), static_expr(child(n, 4)), false))
            case List("non_empty_static_array_pair_list", "T_COMMA", "static_expr") =>
                non_empty_static_array_pair_list(child(n, 0)) ::: List((None, static_expr(child(n, 2)), false))
            case List("static_expr", "T_DOUBLE_ARROW", "static_expr") =>
                List((Some(static_expr(child(n, 0))), static_expr(child(n, 2)), false))
            case List("static_expr") =>
                List((None, static_expr(child(n, 0)), false))
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
                VariableVariable(variable(child(n, 1))).setPos(child(n, 0))
            case List("T_DOLLAR", "T_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                VariableVariable(expr(child(n, 1))).setPos(child(n, 0))
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
                      inner_statement_list(child(n, 6))).setPos(child(n, 0))
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
            case List("variable") => (variable_w(child(n)), false)
            case List("T_BITWISE_AND", "variable") => (variable_w(child(n, 1)), true)
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
                List(identifier(child(n)))
            case List("namespace_name", "T_NS_SEPARATOR", "T_STRING") =>
                namespace_name(child(n, 0)) ::: List(identifier(child(n, 2)))
        }
    }

    def fully_qualified_class_name(n: ParseNode): StaticClassRef = {
        var root: NSRoot = NSNone
        var c: ParseNode = childrenNames(n) match {
            case List("namespace_name") => root = NSNone; child(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") => root = NSCurrent; child(n, 2)
            case List("T_NS_SEPARATOR", "namespace_name") => root = NSGlobal; child(n, 1)
        }

        root.setPos(child(n, 0))
        val parts = namespace_name(c);
        StaticClassRef(root, parts.init, parts.last).setPos(root)
    }

    def variable_list(n: ParseNode): List[Variable] = {
        childrenNames(n) match {
            case List("variable") => 
                List(variable_u(child(n)))
            case List("variable_list", "T_COMMA", "variable") =>
                variable_list(child(n, 0)) ::: List(variable_u(child(n, 2)))
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
            case (expr, stat) :: rest => Some(If(expr, stat, elseif_else(rest, elze)).setPos(expr))
            case Nil => elze
        }
    }

    def function_declaration_statement(n: ParseNode): FunctionDecl = {
        childrenNames(n) match {
            case List("T_FUNCTION", "is_reference", "T_STRING", "T_OPEN_BRACES", "parameter_list", "T_CLOSE_BRACES", "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") =>
                val pos = new Position().setPos(child(n, 2));
                val com = comp.getPreviousComment(pos);

                FunctionDecl(identifier(child(n, 2)), parameter_list(child(n, 4)), is_reference(child(n, 1)), None, inner_statement_list(child(n, 7))).setPos(pos).attachComment(com)
        }
    }

    def expr(n: ParseNode): Expression = {
        val pos = new Position().setPos(child(n, 0));
        val com = comp.getPreviousComment(pos);

        (childrenNames(n) match {
            case List("variable") =>
                variable(child(n))
            case List("variable", "T_ASSIGN", "expr") =>
                Assign(variable_w(child(n, 0)), expr(child(n, 2)), false).setPos(child(n, 0))
            case List("variable", "T_ASSIGN", "T_BITWISE_AND", "variable") =>
                Assign(variable_w(child(n, 0)), variable(child(n, 3)), true)
            case List("T_NEW", "class_name_reference", "ctor_arguments") =>
                New(class_name_reference(child(n, 1)), ctor_arguments(child(n, 2)))
            case List("T_CLONE", "expr") =>
                Clone(expr(child(n, 1)))
            case List("variable", "T_ASSIGN", "T_BITWISE_AND", "T_NEW", "class_name_reference", "ctor_arguments") =>
                Assign(variable_w(child(n, 0)), New(class_name_reference(child(n, 4)), ctor_arguments(child(n, 5))).setPos(child(n, 3)), true)
            case List("variable", "T_PLUS_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), Plus(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_MINUS_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), Minus(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_MUL_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), Mult(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_DIV_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), Div(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_CONCAT_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), Concat(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_MOD_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), Mod(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_AND_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), BitwiseAnd(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_OR_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), BitwiseOr(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_XOR_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), BitwiseXor(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_SL_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), ShiftLeft(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_SR_EQUAL", "expr") =>
                Assign(variable_w(child(n, 0)), ShiftRight(variable(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)), false)
            case List("variable", "T_INC") => PostInc(variable_w(child(n, 0)))
            case List("T_INC", "variable") => PreInc(variable_w(child(n, 1)))
            case List("variable", "T_DEC") => PostDec(variable_w(child(n, 0)))
            case List("T_DEC", "variable") => PreDec(variable_w(child(n, 1)))
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
            case List("T_MINUS", "expr") =>
                Minus(PHPInteger(0), expr(child(n, 1)))
            case List("T_PLUS", "expr") =>
                Plus(PHPInteger(0), expr(child(n, 1)))
            case List("T_NOT", "expr") =>
                BooleanNot(expr(child(n, 1)))
            case List("T_AT", "expr") =>
                Silence(expr(child(n, 1)))
            case List("T_BITWISE_NOT", "expr") =>
                BitwiseNot(expr(child(n, 1)))
            case List("expr", "T_IS_IDENTICAL", "expr") =>
                Identical(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_NOT_IDENTICAL", "expr") =>
                BooleanNot(Identical(expr(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)))
            case List("expr", "T_IS_EQUAL", "expr") =>
                Equal(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_NOT_EQUAL", "expr") =>
                BooleanNot(Equal(expr(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)))
            case List("expr", "T_IS_SMALLER", "expr") =>
                Smaller(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_SMALLER_OR_EQUAL", "expr") =>
                SmallerEqual(expr(child(n, 0)), expr(child(n, 2)))
            case List("expr", "T_IS_GREATER", "expr") =>
                BooleanNot(SmallerEqual(expr(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)))
            case List("expr", "T_IS_GREATER_OR_EQUAL", "expr") =>
                BooleanNot(Smaller(expr(child(n, 0)), expr(child(n, 2))).setPos(child(n, 1)))
            case List("expr", "T_INSTANCEOF", "class_name_reference") =>
                InstanceOf(expr(child(n, 0)), class_name_reference(child(n, 2)))
            case List("T_OPEN_BRACES", "expr", "T_CLOSE_BRACES") =>
                expr(child(n, 1))
            case List("expr", "T_QUESTION", "expr", "T_COLON", "expr") =>
                Ternary(expr(child(n, 0)), Some(expr(child(n, 2))), expr(child(n, 4)))
            case List("expr", "T_QUESTION", "T_COLON", "expr") =>
                Ternary(expr(child(n, 0)), None, expr(child(n, 2)))
            case List("internal_functions_in_yacc") =>
                internal_functions_in_yacc(child(n, 0))
            case List("T_INT_CAST", "expr") =>
                Cast(CastInt.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_DOUBLE_CAST", "expr") =>
                Cast(CastDouble.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_STRING_CAST", "expr") =>
                Cast(CastString.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_ARRAY_CAST", "expr") =>
                Cast(CastArray.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_OBJECT_CAST", "expr") =>
                Cast(CastObject.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_BOOL_CAST", "expr") =>
                Cast(CastBool.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_UNSET_CAST", "expr") =>
                Cast(CastUnset.setPos(child(n, 0)), expr(child(n, 1)))
            case List("T_EXIT", "exit_expr") =>
                Exit(exit_expr(child(n, 1)))
            case List("T_AT", " expr") =>
                Silence(expr(child(n, 1)))
            case List("scalar") =>
                scalar(child(n,0))
            case List("T_ARRAY", "T_OPEN_BRACES", "array_pair_list", "T_CLOSE_BRACES") =>
                Array(array_pair_list(child(n, 2)))
            case List("T_LIST", "T_OPEN_BRACES", "assignment_list", "T_CLOSE_BRACES", "T_ASSIGN", "expr") =>
                ExpandArray(assignment_list(child(n, 2)), expr(child(n, 5)))
            case List("T_BACKTICK", "backticks_expr", "T_BACKTICK") =>
                Execute("...");
            case List("T_PRINT", "expr") =>
                Print(expr(child(n, 1)))
            case List("T_FUNCTION", "is_reference", "T_OPEN_BRACES", "parameter_list", "T_CLOSE_BRACES", "lexical_vars", "T_OPEN_CURLY_BRACES", "inner_statement_list", "T_CLOSE_CURLY_BRACES") =>
                val pos = new Position().setPos(child(n, 2));
                val com = comp.getPreviousComment(pos);

                Closure(parameter_list(child(n, 3)), lexical_vars(child(n, 5)), is_reference(child(n, 1)), None, inner_statement_list(child(n, 7))).setPos(pos).attachComment(com)
            case List("base_variable_with_function_calls") =>
                base_variable_with_function_calls(child(n))
            case _ => unspecified(n)
        }).setPos(pos).attachComment(com)
    }

    def lexical_vars(n: ParseNode): List[ArgumentDecl] = childrenNames(n) match {
        case List() =>
            List()
        case List("T_USE", "T_OPEN_BRACES", "lexical_var_list", "T_CLOSE_BRACES") =>
            lexical_var_list(child(n, 2))
    }

    def lexical_var_list(n: ParseNode): List[ArgumentDecl] = childrenNames(n) match {
        case List("lexical_var_list", "T_COMMA", "T_VARIABLE") =>
            lexical_var_list(child(n)) ::: List(ArgumentDecl(t_variable(child(n, 2)), None, None, false))
        case List("lexical_var_list", "T_COMMA", "T_BITWISE_AND", "T_VARIABLE") =>
            lexical_var_list(child(n)) ::: List(ArgumentDecl(t_variable(child(n, 3)), None, None, true))
        case List("T_VARIABLE") =>
            List(ArgumentDecl(t_variable(child(n, 0)), None, None, false))
        case List("T_BITWISE_AND", "T_VARIABLE") =>
            List(ArgumentDecl(t_variable(child(n, 1)), None, None, true))
    }

    def assignment_list(n: ParseNode): List[Option[Variable]] = {
        childrenNames(n) match {
            case List("assignment_list", "T_COMMA", "assignment_list_element") =>
                assignment_list(child(n, 0)) ::: List(assignment_list_element(child(n, 2)))
            case List("assignment_list_element") =>
                List(assignment_list_element(child(n, 0)))
        }
    }

    def assignment_list_element(n: ParseNode): Option[Variable] = {
        childrenNames(n) match {
            case List("variable") =>
                Some(variable_w(child(n)))
            case List("T_LIST", "T_OPEN_BRACES",  "assignment_list", "T_CLOSE_BRACES") =>
                println("TODO: Nested list statements are not supported yet");
                None
            case List() =>
                None
        }
    }

    def ctor_arguments(n: ParseNode): List[CallArg] = {
        childrenNames(n) match {
            case List() => 
                List()
            case List("T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                function_call_parameter_list(child(n, 1))
        }
    }

    def class_name_reference(n: ParseNode): ClassRef = {
        childrenNames(n) match {
            case List("class_name") =>
                class_name(child(n))
            case List("dynamic_class_name_reference") =>
                dynamic_class_name_reference(child(n))
        }
    }

    // Derive $a->foo->bar[]->gee->stuff()->bla into an expression
    def deriveOAList(baseex: Expression, oaList: List[ObjectAccess]) = {
        var ex = baseex
        for(val oa <- oaList) {
            oa match {
                case OAIdentifier(id) => ex = ObjectProperty(ex, id).setPos(id)
                case OAArray(array, indexes) => for (val id <- indexes) id match {
                        case Some(i) => ex = ArrayEntry(ex, i).setPos(ex)
                        case None => ex = NextArrayEntry(ex).setPos(ex)
                    }
                case OAExpression(exp) => ex = DynamicObjectProperty(ex, exp).setPos(ex)
                case OAMethod(name, args) => name match {
                    case OAIdentifier(id) => ex = MethodCall(ex, StaticMethodRef(id).setPos(id), args).setPos(id)
                    case OAExpression(e)  => ex = MethodCall(ex, DynamicMethodRef(e).setPos(e), args).setPos(ex)
                    case OAArray(array, indexes) =>  {
                        for (val id <- indexes) id match {
                            case Some(i) => ex = ArrayEntry(ex, i).setPos(ex)
                            case None => ex = NextArrayEntry(ex).setPos(ex)
                        }

                        ex = FunctionCall(DynamicFunctionRef(ex).setPos(ex), args).setPos(ex)
                    }
                }
            }
        }
        ex
    }

    def dynamic_class_name_reference(n: ParseNode): ClassRef = {
        childrenNames(n) match {
            case List("base_variable", "T_OBJECT_OPERATOR", "object_property", "dynamic_class_name_variable_properties") =>
                val oaList: List[ObjectAccess] = List(object_property(child(n, 2))) ::: dynamic_class_name_variable_properties(child(n, 3))
                DynamicClassRef(deriveOAList(base_variable(child(n, 0)), oaList))
            case List("base_variable") =>
                val bv = base_variable(child(n))
                VarClassRef(bv).setPos(bv)
        }
    }

    def dynamic_class_name_variable_properties(n: ParseNode): List[ObjectAccess] = {
        childrenNames(n) match {
            case List("dynamic_class_name_variable_properties", "T_OBJECT_OPERATOR", "object_property") =>
                dynamic_class_name_variable_properties(child(n, 0)) ::: List(object_property(child(n, 2)))
            case List() =>
                List()
        }
    }

    def method_or_not(n: ParseNode): Option[List[CallArg]] = {
        childrenNames(n) match {
            case List("T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") => Some(function_call_parameter_list(child(n, 1)))
            case List() => None
        }
    }

    def function_call_parameter_list(n: ParseNode): List[CallArg] = {
        childrenNames(n) match {
            case List("non_empty_function_call_parameter_list") => non_empty_function_call_parameter_list(child(n))
            case List() => List()
        }
    }

    def non_empty_function_call_parameter_list(n: ParseNode): List[CallArg] = {
        childrenNames(n) match {
            case List("expr") => 
                val ex = expr(child(n))
                List(CallArg(ex, false).setPos(ex))
            case List("T_BITWISE_AND", "variable") =>
                val v = variable(child(n, 1))
                List(CallArg(v, true).setPos(child(n, 0)))
            case List("non_empty_function_call_parameter_list", "T_COMMA", "expr") =>
                val ex = expr(child(n, 2))
                non_empty_function_call_parameter_list(child(n, 0)) ::: List(CallArg(ex, false).setPos(ex))
            case List("non_empty_function_call_parameter_list", "T_COMMA", "T_BITWISE_AND", "variable") =>
                val v = variable(child(n, 3))
                non_empty_function_call_parameter_list(child(n, 0)) ::: List(CallArg(v, true).setPos(child(n, 2)))
        }
    }


    def variable_w(n: ParseNode): Variable = {
        variable(n) match {
            case v: Variable => v
            case _ => error("Cannot write to non-variable");
        }
    }

    def variable_u(n: ParseNode): Variable = {
        variable(n) match {
            case v: Variable => v
            case _ => error("Unnexpected non-variable, expecting variable");
        }
    }
    def variable(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("base_variable_with_function_calls", "T_OBJECT_OPERATOR", "object_property", "method_or_not", "variable_properties") => 
                val oaList: List[ObjectAccess] = List(object_property_method(child(n, 2), child(n, 3))) ::: variable_properties(child(n, 4))
                deriveOAList(base_variable_with_function_calls(child(n, 0)), oaList);
            case List("base_variable_with_function_calls") => 
                base_variable_with_function_calls(child(n))
        }
    }

    def object_property_method(op: ParseNode, mon: ParseNode): ObjectAccess = {
        method_or_not(mon) match {
            case Some(args) => OAMethod(object_property(op), args).setPos(op);
            case None => object_property(op)
        }
    }

    def variable_properties(n: ParseNode): List[ObjectAccess] = {
        childrenNames(n) match {
            case List("variable_properties", "variable_property") =>
                variable_properties(child(n, 0)) ::: List(variable_property(child(n, 1)))
            case List() =>
                List()
        }
    }

    def variable_property(n: ParseNode): ObjectAccess = {
        childrenNames(n) match {
            case List("T_OBJECT_OPERATOR", "object_property", "method_or_not") =>
                object_property_method(child(n, 1), child(n, 2))
        }
    }

    def base_variable_with_function_calls(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("base_variable") => base_variable(child(n))
            case List("function_call") => function_call(child(n))
        }
    }

    def function_call(n: ParseNode): Expression = {
        (childrenNames(n) match {
            case List("namespace_name", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                val parts = namespace_name(child(n, 0))
                FunctionCall(StaticFunctionRef(NSNone.setPos(child(n, 0)).setPos(child(n, 0)), parts.init, parts.last), function_call_parameter_list(child(n, 2)))
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                val parts = namespace_name(child(n, 2))
                FunctionCall(StaticFunctionRef(NSCurrent.setPos(child(n, 0)).setPos(child(n, 0)), parts.init, parts.last), function_call_parameter_list(child(n, 4)))
            case List("T_NS_SEPARATOR", "namespace_name", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                val parts = namespace_name(child(n, 1))
                FunctionCall(StaticFunctionRef(NSGlobal.setPos(child(n, 0)).setPos(child(n, 0)), parts.init, parts.last), function_call_parameter_list(child(n, 4)))
            case List("class_name", "T_DOUBLE_COLON", "T_STRING", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                StaticMethodCall(class_name(child(n, 0)), StaticMethodRef(identifier(child(n, 2))).setPos(child(n, 2)), function_call_parameter_list(child(n, 4)))
            case List("class_name", "T_DOUBLE_COLON", "variable_without_objects", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                StaticMethodCall(class_name(child(n, 0)), DynamicMethodRef(variable_without_objects(child(n, 2))).setPos(child(n, 2)), function_call_parameter_list(child(n, 4)))
            case List("reference_variable", "T_DOUBLE_COLON", "T_STRING", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                StaticMethodCall(DynamicClassRef(reference_variable(child(n, 0))).setPos(child(n, 0)), StaticMethodRef(identifier(child(n, 2))).setPos(child(n, 2)), function_call_parameter_list(child(n, 4)))
            case List("reference_variable", "T_DOUBLE_COLON", "variable_without_objects", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                StaticMethodCall(DynamicClassRef(reference_variable(child(n, 0))).setPos(child(n, 0)), DynamicMethodRef(variable_without_objects(child(n, 2))).setPos(child(n, 2)), function_call_parameter_list(child(n, 4)))
            case List("variable_without_objects", "T_OPEN_BRACES", "function_call_parameter_list", "T_CLOSE_BRACES") =>
                FunctionCall(VarFunctionRef(variable_without_objects(child(n, 0))).setPos(child(n, 0)), function_call_parameter_list(child(n, 2)))
        }).setPos(child(n, 0))
    }

    def base_variable(n: ParseNode): Variable = {
        childrenNames(n) match {
            case List("reference_variable") => 
                reference_variable(child(n))
            case List("simple_indirect_reference", "reference_variable") =>
                var r = reference_variable(child(n, 1))
                val i = simple_indirect_reference(child(n, 0))
                for (n <- 0 until i) r = VariableVariable(r).setPos(r)
                r
            case List("static_member") =>
                static_member(child(n))
        }
    }

    def static_member(n: ParseNode) = {
        (childrenNames(n) match {
            case List("class_name", "T_DOUBLE_COLON", "variable_without_objects") =>
                ClassProperty(class_name(child(n, 0)), variable_without_objects(child(n, 2))).setPos(child(n, 0))
            case List("reference_variable", "T_DOUBLE_COLON", "variable_without_objects") =>
                ClassProperty(VarClassRef(reference_variable(child(n, 0))).setPos(child(n, 0)), variable_without_objects(child(n, 2)))
        }).setPos(child(n, 0))
    }

    def object_property(n: ParseNode): ObjectAccess = {
        childrenNames(n) match {
            case List("object_dim_list") => object_dim_list(child(n))
            case List("variable_without_objects") => val vwo = variable_without_objects(child(n)); OAExpression(vwo).setPos(vwo)
        }
    }

    def object_dim_list(n: ParseNode): ObjectAccess = {
        def accumulate(n: ParseNode, acc: List[Option[Expression]]): (OAScalar, List[Option[Expression]]) = childrenNames(n) match {
            case List("object_dim_list", "T_OPEN_RECT_BRACES", "dim_offset", "T_CLOSE_RECT_BRACES") =>
                accumulate(child(n, 0), dim_offset(child(n, 2)) :: acc)
            case List("object_dim_list", "T_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                accumulate(child(n, 0), Some(expr(child(n, 2))) :: acc)
            case List("variable_name") =>
                (variable_name(child(n)), acc)
        }

        accumulate(n, Nil) match {
            case (x, Nil) => x
            case (x, xs) => OAArray(x, xs).setPos(x)
        }

    }

    def variable_name(n: ParseNode): OAScalar = {
        childrenNames(n) match {
            case List("T_STRING") =>
                val i = identifier(child(n))
                OAIdentifier(i).setPos(i)
            case List("T_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                val ex = expr(child(n, 1))
                OAExpression(ex).setPos(ex)
        }
    }

    def dim_offset(n: ParseNode): Option[Expression] = {
        childrenNames(n) match {
            case List() => None
            case List("expr") => Some(expr(child(n)))
        }
    }

    def reference_variable(n: ParseNode): Variable = {
        childrenNames(n) match {
            case List("reference_variable", "T_OPEN_RECT_BRACES", "dim_offset", "T_CLOSE_RECT_BRACES") => val rv = reference_variable(child(n, 0)); dim_offset(child(n, 2)) match {
                case Some(ex) => ArrayEntry(rv, ex).setPos(rv)
                case None => NextArrayEntry(rv).setPos(rv)
            }
            case List("reference_variable", "T_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                    val rv = reference_variable(child(n, 0));
                    ArrayEntry(rv, expr(child(n, 2))).setPos(rv)
            case List("compound_variable") =>
                compound_variable(child(n))
        }
    }

    def compound_variable(n: ParseNode): Variable = {
        childrenNames(n) match {
            case List("T_VARIABLE") =>
                t_variable(child(n))
            case List("T_DOLLAR", "T_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                val e = expr(child(n, 2));
                VariableVariable(e).setPos(e)
        }
    }

    def variable_without_objects(n: ParseNode): Variable = {
        childrenNames(n) match {
            case List("reference_variable") => reference_variable(child(n))
            case List("simple_indirect_reference", "reference_variable") =>
                var r = reference_variable(child(n, 1))
                val i = simple_indirect_reference(child(n, 0))
                for (n <- 0 until i) r = VariableVariable(r).setPos(r)
                r
        }
    }

    def simple_indirect_reference(n: ParseNode): Int = {
        childrenNames(n) match {
            case List("T_DOLLAR") => 
                1
            case List("simple_indirect_reference", "T_DOLLAR") =>
                simple_indirect_reference(child(n, 0)) + 1
        }
    }

    def scalar(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_STRING_VARNAME") =>
                PHPString(child(n).tokenContent).setPos(child(n))
            case List("class_constant") =>
                class_constant(child(n))
            case List("namespace_name") =>
                static_constant(n)
            case List("T_NAMESPACE", "T_NS_SEPARATOR", "namespace_name") =>
                static_constant(n)
            case List("T_NS_SEPARATOR", "namespace_name") =>
                static_constant(n)
            case List("common_scalar") =>
                common_scalar(child(n))
            case List("T_DOUBLE_QUOTE", "encaps_list", "T_DOUBLE_QUOTE") => 
                encaps_list(child(n, 1))
            case List("T_START_HEREDOC", "encaps_list", "T_END_HEREDOC") =>
                encaps_list(child(n, 1))
            case List("T_DOUBLE_QUOTE", "T_ENCAPSED_AND_WHITESPACE", "T_DOUBLE_QUOTE") => 
                PHPString(child(n, 1).tokenContent).setPos(child(n, 0))
            case List("T_START_HEREDOC", "T_ENCAPSED_AND_WHITESPACE", "T_END_HEREDOC") =>
                PHPString(child(n, 1).tokenContent).setPos(child(n, 0))
        }
    }

    def class_constant(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("class_name", "T_DOUBLE_COLON", "T_STRING") =>
                val cn = class_name(child(n, 0));
                ClassConstant(cn, identifier(child(n, 2))).setPos(cn)
            case List("reference_variable", "T_DOUBLE_COLON", "T_STRING") =>
                val rv = reference_variable(child(n, 0));
                ClassConstant(VarClassRef(rv).setPos(rv), identifier(child(n, 2))).setPos(rv)
        }
    }

    def encaps_list(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("encaps_list", "encaps_var") =>
                val el = encaps_list(child(n, 0))
                Concat(el, encaps_var(child(n, 1))).setPos(el)
            case List("encaps_list", "T_ENCAPSED_AND_WHITESPACE") =>
                val el = encaps_list(child(n, 0))
                Concat(el, PHPString(child(n, 1).tokenContent).setPos(child(n, 1))).setPos(el)
            case List("encaps_var") =>
                encaps_var(child(n, 0))
            case List("T_ENCAPSED_AND_WHITESPACE", "encaps_var") =>
                val l = child(n, 0)
                Concat(PHPString(l.tokenContent).setPos(l), encaps_var(child(n, 1))).setPos(l)
        }
    }

    def encaps_var(n: ParseNode): Expression = {
        (childrenNames(n) match {
            case List("T_VARIABLE") =>
                t_variable(child(n))
            case List("T_VARIABLE", "T_OPEN_RECT_BRACES", "encaps_var_offset", "T_CLOSE_RECT_BRACES") =>
                ArrayEntry(t_variable(child(n, 0)), encaps_var_offset(child(n, 2))).setPos(child(n, 0))
            case List("T_VARIABLE", "T_OBJECT_OPERATOR", "T_STRING") =>
                ObjectProperty(t_variable(child(n, 0)), identifier(child(n, 2))).setPos(child(n, 0))
            case List("T_DOLLAR_OPEN_CURLY_BRACES", "expr", "T_CLOSE_CURLY_BRACES") =>
                VariableVariable(expr(child(n, 1))).setPos(child(n, 1))
            case List("T_DOLLAR_OPEN_CURLY_BRACES", "T_STRING_VARNAME", "T_OPEN_RECT_BRACES", "expr", "T_CLOSE_RECT_BRACES", "T_CLOSE_CURLY_BRACES") =>
                ArrayEntry(SimpleVariable(identifier(child(n, 1))).setPos(child(n, 1)), expr(child(n, 3)))
            case List("T_CURLY_OPEN", "variable", "T_CLOSE_CURLY_BRACES") =>
                variable(child(n, 1))
        }).setPos(child(n, 0))
    }

    def identifier(n: ParseNode): Identifier = {
        Identifier(n.tokenContent).setPos(n.line, n.column, n.file)
    }

    def varIdentifier(n: ParseNode): Identifier = {
        Identifier(n.tokenContent.substring(1)).setPos(n.line, n.column, n.file)
    }

    def t_variable(n: ParseNode): SimpleVariable = {
        SimpleVariable(Identifier(n.tokenContent.substring(1)).setPos(n.line, n.column, n.file)).setPos(n.line, n.column, n.file)
    }

    def encaps_var_offset(n: ParseNode): Expression = {
        childrenNames(n) match {
            case List("T_STRING") => PHPString(child(n).tokenContent).setPos(child(n, 0))
            case List("T_NUM_STRING") => PHPString(child(n).tokenContent).setPos(child(n, 0))
            case List("T_VARIABLE") => t_variable(child(n))
        }
    }

    def internal_functions_in_yacc(n: ParseNode): Expression = {
        val pos = new Position().setPos(child(n, 0))
        val com = comp.getPreviousComment(pos);

        (childrenNames(n) match {
            case List("T_ISSET", "T_OPEN_BRACES", "isset_variables", "T_CLOSE_BRACES") =>
                Isset(isset_variables(child(n, 2)))
            case List("T_EMPTY", "T_OPEN_BRACES", "variable", "T_CLOSE_BRACES") =>
                Empty(variable_u(child(n, 2)))
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
        }).setPos(pos).attachComment(com)
    }

    def isset_variables(n: ParseNode): List[Variable] = {
        childrenNames(n) match {
            case List("variable") => List(variable_u(child(n, 0)))
            case List("isset_variables", "T_COMMA", "variable") => isset_variables(child(n, 0)) ::: List(variable_u(child(n, 2)))
        }
    }

    def array_pair_list(n: ParseNode): List[(Option[Expression], Expression, Boolean)] = {
        childrenNames(n) match {
            case List() => 
                List()
            case List("non_empty_array_pair_list", "possible_comma") =>
                non_empty_array_pair_list(child(n, 0))
        }
    }

    def non_empty_array_pair_list(n: ParseNode): List[(Option[Expression], Expression, Boolean)] = {
        childrenNames(n) match {
            case List("non_empty_array_pair_list", "T_COMMA", "expr", "T_DOUBLE_ARROW", "expr") =>
                non_empty_array_pair_list(child(n, 0)) ::: List((Some(expr(child(n, 2))), expr(child(n, 4)), false))
            case List("non_empty_array_pair_list", "T_COMMA", "expr") =>
                non_empty_array_pair_list(child(n, 0)) ::: List((None, expr(child(n, 2)), false))
            case List("non_empty_array_pair_list", "T_COMMA", "expr", "T_DOUBLE_ARROW", "T_BITWISE_AND", "variable") =>
                non_empty_array_pair_list(child(n, 0)) ::: List((Some(expr(child(n, 2))), variable(child(n, 5)), true))
            case List("non_empty_array_pair_list", "T_COMMA", "T_BITWISE_AND", "variable") =>
                non_empty_array_pair_list(child(n, 0)) ::: List((None, variable(child(n, 3)), true))
            case List("expr", "T_DOUBLE_ARROW", "expr") =>
                List((Some(expr(child(n, 0))), expr(child(n, 2)), false))
            case List("expr") =>
                List((None, expr(child(n, 0)), false))
            case List("expr", "T_DOUBLE_ARROW", "T_BITWISE_AND", "variable") =>
                List((Some(expr(child(n, 0))), variable(child(n, 3)), true))
            case List("T_BITWISE_AND", "variable") =>
                List((None, variable(child(n, 1)), true))
        }
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
