package phantm
package parser

import edu.tum.cup2.grammar.{NonTerminal, Terminal}
import edu.tum.cup2.semantics.{SymbolValue}

import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}

import ast.Trees._

case class Yytoken(tpe: Terminals.Value, line: Int, column: Int, content: String)

object Terminals extends SymbolEnum {
  val T_ABSTRACT,
      T_AND_EQUAL,
      T_ARRAY,
      T_ARRAY_CAST,
      T_AS,
      T_ASSIGN,
      T_AT,
      T_BACKTICK,
      T_BITWISE_AND,
      T_BITWISE_NOT,
      T_BITWISE_OR,
      T_BITWISE_XOR,
      T_BOOLEAN_AND,
      T_BOOLEAN_OR,
      T_BOOL_CAST,
      T_BREAK,
      T_CASE,
      T_CATCH,
      T_CLASS,
      T_CLASS_C,
      T_CLONE,
      T_CLOSE_BRACES,
      T_CLOSE_CURLY_BRACES,
      T_CLOSE_RECT_BRACES,
      T_COLON,
      T_COMMA,
      T_CONCAT_EQUAL,
      T_CONST,
      T_CONSTANT_ENCAPSED_STRING,
      T_CONTINUE,
      T_CURLY_OPEN,
      T_DEC,
      T_DECLARE,
      T_DEFAULT,
      T_DIR,
      T_DIV,
      T_DIV_EQUAL,
      T_DNUMBER,
      T_DO,
      T_DOLLAR,
      T_DOLLAR_OPEN_CURLY_BRACES,
      T_DOUBLE_ARROW,
      T_DOUBLE_CAST,
      T_DOUBLE_QUOTE,
      T_ECHO,
      T_ELSE,
      T_ELSEIF,
      T_EMPTY,
      T_ENCAPSED_AND_WHITESPACE,
      T_ENDDECLARE,
      T_ENDFOR,
      T_ENDFOREACH,
      T_ENDIF,
      T_ENDSWITCH,
      T_ENDWHILE,
      T_END_HEREDOC,
      T_EVAL,
      T_EXIT,
      T_EXTENDS,
      T_FILE,
      T_FINAL,
      T_FOR,
      T_FOREACH,
      T_FUNCTION,
      T_FUNC_C,
      T_GLOBAL,
      T_GOTO,
      T_HALT_COMPILER,
      T_IF,
      T_IMPLEMENTS,
      T_INC,
      T_INCLUDE,
      T_INCLUDE_ONCE,
      T_INLINE_HTML,
      T_INSTANCEOF,
      T_INTERFACE,
      T_INT_CAST,
      T_ISSET,
      T_IS_EQUAL,
      T_IS_GREATER,
      T_IS_GREATER_OR_EQUAL,
      T_IS_IDENTICAL,
      T_IS_NOT_EQUAL,
      T_IS_NOT_IDENTICAL,
      T_IS_SMALLER,
      T_IS_SMALLER_OR_EQUAL,
      T_LINE,
      T_LIST,
      T_LNUMBER,
      T_LOGICAL_AND,
      T_LOGICAL_OR,
      T_LOGICAL_XOR,
      T_METHOD_C,
      T_MINUS,
      T_MINUS_EQUAL,
      T_MODULO,
      T_MOD_EQUAL,
      T_MULT,
      T_MUL_EQUAL,
      T_NAMESPACE,
      T_NEW,
      T_NOT,
      T_NS_C,
      T_NS_SEPARATOR,
      T_NUM_STRING,
      T_OBJECT_CAST,
      T_OBJECT_OPERATOR,
      T_OPEN_BRACES,
      T_OPEN_CURLY_BRACES,
      T_OPEN_RECT_BRACES,
      T_OR_EQUAL,
      T_DOUBLE_COLON,
      T_PLUS,
      T_PLUS_EQUAL,
      T_POINT,
      T_PRINT,
      T_PRIVATE,
      T_PROTECTED,
      T_PUBLIC,
      T_QUESTION,
      T_REQUIRE,
      T_REQUIRE_ONCE,
      T_RETURN,
      T_SEMICOLON,
      T_SL,
      T_SL_EQUAL,
      T_SR,
      T_SR_EQUAL,
      T_START_HEREDOC,
      T_STATIC,
      T_STRING,
      T_STRING_CAST,
      T_STRING_VARNAME,
      T_SWITCH,
      T_THROW,
      T_TRY,
      T_UNSET,
      T_UNSET_CAST,
      T_USE,
      T_VAR,
      T_VARIABLE,
      T_WHILE,
      T_XOR_EQUAL = TerminalEnum;
}


class PHP53Spec extends CUP2Specification with ScalaCUPSpecification {

    /*
    Translation of Bison character tokens to named tokens
    *******************************************************

    ,   T_COMMA
    =   T_ASSIGN
    ?   T_QUESTION
    :   T_COLON
    |   T_BITWISE_OR
    ^   T_BITWISE_XOR
    &   T_BITWISE_AND
    <   T_IS_SMALLER
    >   T_IS_GREATER
    +   T_PLUS
    -   T_MINUS
    .   T_POINT
    *   T_MULT
    /   T_DIV
    %   T_MODULO
    !   T_NOT
    ~   T_BITWISE_NOT
    @   T_AT
    {   T_OPEN_CURLY_BRACES
    }   T_CLOSE_CURLY_BRACES
    (   T_OPEN_BRACES
    )   T_CLOSE_BRACES
    ;   T_SEMICOLON
    $   T_DOLLAR
    `   T_BACKTICK
    "   T_DOUBLE_QUOTE
    '   T_SINGLE_QUOTE
    [   T_OPEN_RECT_BRACES
    ]   T_CLOSE_RECT_BRACES
    */
  /**
   * Enum workaround in Scala for NonTerminal-Definition
   */
  object NonTerminals extends SymbolEnum {
    val S,
        additional_catch,
        additional_catches,
        array_pair_list,
        assignment_list,
        assignment_list_element,
        backticks_expr,
        base_variable,
        base_variable_with_function_calls,
        case_list,
        case_separator,
        class_constant,
        class_constant_declaration,
        class_declaration_statement,
        class_entry_type,
        class_name,
        class_name_reference,
        class_statement,
        class_statement_list,
        class_variable_declaration,
        common_scalar,
        compound_variable,
        constant_declaration,
        ctor_arguments,
        declare_list,
        declare_statement,
        dim_offset,
        dynamic_class_name_reference,
        dynamic_class_name_variable_properties,
        echo_expr_list,
        else_single,
        elseif_list,
        encaps_list,
        encaps_var,
        encaps_var_offset,
        exit_expr,
        expr,
        extends_from,
        for_expr,
        for_statement,
        foreach_optional_arg,
        foreach_statement,
        foreach_variable,
        fully_qualified_class_name,
        function_call,
        function_call_parameter_list,
        function_declaration_statement,
        global_var,
        global_var_list,
        implements_list,
        inner_statement,
        inner_statement_list,
        interface_extends_list,
        interface_list,
        internal_functions_in_yacc,
        is_reference,
        isset_variables,
        lexical_var_list,
        lexical_vars,
        member_modifier,
        method_body,
        method_modifiers,
        method_or_not,
        namespace_name,
        new_else_single,
        new_elseif_list,
        non_empty_additional_catches,
        non_empty_array_pair_list,
        non_empty_for_expr,
        non_empty_function_call_parameter_list,
        non_empty_member_modifiers,
        non_empty_parameter_list,
        non_empty_static_array_pair_list,
        object_dim_list,
        object_property,
        optional_class_type,
        parameter_list,
        possible_comma,
        variable,
        reference_variable,
        scalar,
        simple_indirect_reference,
        statement,
        static_array_pair_list,
        static_class_constant,
        static_member,
        static_expr,
        static_var_list,
        switch_case_list,
        top_statement,
        top_statement_list,
        use_declaration,
        use_declarations,
        variable_list,
        variable_modifiers,
        variable_name,
        variable_properties,
        variable_property,
        variable_without_objects,
        while_statement = NonTerminalEnum
  }

  // make the enum-Values available
  import NonTerminals._, Terminals._

  // tell parent class what (non)terminals exist
  val terminals = Terminals
  val nonTerminals = NonTerminals

  precedences(
      left(T_INCLUDE, T_INCLUDE_ONCE, T_EVAL, T_REQUIRE, T_REQUIRE_ONCE),
      left(T_COMMA),
      left(T_LOGICAL_OR),
      left(T_LOGICAL_XOR),
      left(T_LOGICAL_AND),
      right(T_PRINT),
      left(T_ASSIGN, T_PLUS_EQUAL, T_MINUS_EQUAL, T_MUL_EQUAL, T_DIV_EQUAL, T_CONCAT_EQUAL, T_MOD_EQUAL, T_AND_EQUAL, T_OR_EQUAL, T_XOR_EQUAL, T_SL_EQUAL, T_SR_EQUAL),
      left(T_QUESTION, T_COLON),
      left(T_BOOLEAN_OR),
      left(T_BOOLEAN_AND),
      left(T_BITWISE_OR),
      left(T_BITWISE_XOR),
      left(T_BITWISE_AND),
      nonassoc(T_IS_EQUAL, T_IS_NOT_EQUAL, T_IS_IDENTICAL, T_IS_NOT_IDENTICAL),
      nonassoc(T_IS_SMALLER, T_IS_SMALLER_OR_EQUAL, T_IS_GREATER, T_IS_GREATER_OR_EQUAL),
      left(T_SL, T_SR),
      left(T_PLUS, T_MINUS, T_POINT),
      left(T_MULT, T_DIV, T_MODULO),
      right(T_NOT),
      nonassoc(T_INSTANCEOF),
      right(T_BITWISE_NOT, T_INC, T_DEC, T_INT_CAST, T_DOUBLE_CAST, T_STRING_CAST, T_ARRAY_CAST, T_OBJECT_CAST, T_BOOL_CAST, T_UNSET_CAST, T_AT),
      right(T_OPEN_RECT_BRACES),
      nonassoc(T_NEW, T_CLONE),
      left(T_ELSEIF),
      left(T_ELSE),
      left(T_ENDIF),
      right(T_STATIC, T_ABSTRACT, T_FINAL, T_PRIVATE, T_PROTECTED, T_PUBLIC)
  );

  class S                               extends SymbolValue[Program]
  class top_statement_list              extends SymbolValue[List[Statement]]
  class top_statement                   extends SymbolValue[Statement]
  class additional_catches              extends SymbolValue[List[Catch]]
  class non_empty_additional_catches    extends SymbolValue[List[Catch]]
  class additional_catch                extends SymbolValue[Catch]
  class variable_list                   extends SymbolValue[List[Variable]]
  class variable                        extends SymbolValue[Variable]
  class is_reference                    extends SymbolValue[Boolean]
  class function_declaration_statement  extends SymbolValue[FunctionDecl]
  class parameter_list                  extends SymbolValue[List[ArgumentDecl]]
  class class_declaration_statement     extends SymbolValue[Statement]
  class class_entry_type                extends SymbolValue[ClassFlag]
  class extends_from                    extends SymbolValue[Option[StaticClassRef]]
  class interface_extends_list          extends SymbolValue[List[StaticClassRef]]
  class implements_list                 extends SymbolValue[List[StaticClassRef]]
  class interface_list                  extends SymbolValue[List[StaticClassRef]]
  class foreach_optional_arg            extends SymbolValue[Option[(Variable, Boolean)]]
  class foreach_variable                extends SymbolValue[(Variable, Boolean)]
  class for_statement                   extends SymbolValue[Statement]
  class foreach_statement               extends SymbolValue[Statement]
  class declare_statement               extends SymbolValue[Statement]
  class declare_list                    extends SymbolValue[List[String]]
  class switch_case_list                extends SymbolValue[List[(Option[Expression], Statement)]]
  class case_list                       extends SymbolValue[List[(Option[Expression], Statement)]]
  class case_separator                  extends SymbolValue[Option[Nothing]]
  class while_statement                 extends SymbolValue[Statement]
  class elseif_list                     extends SymbolValue[List[(Expression, Statement)]]
  class new_elseif_list                 extends SymbolValue[List[(Expression, Statement)]]
  class else_single                     extends SymbolValue[Option[Statement]]
  class new_else_single                 extends SymbolValue[Option[Statement]]
  class non_empty_parameter_list        extends SymbolValue[List[ArgumentDecl]]
  class optional_class_type             extends SymbolValue[Option[TypeHint]]
  class function_call_parameter_list    extends SymbolValue[List[CallArg]]
  class non_empty_function_call_parameter_list extends SymbolValue[List[CallArg]]
  class global_var_list                  extends SymbolValue[List[Variable]]
  class global_var                       extends SymbolValue[Variable]
  class static_var_list                  extends SymbolValue[List[InitVariable]]
  class class_statement_list             extends SymbolValue[(List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])]
  class class_statement                  extends SymbolValue[(List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])]
  class method_body                      extends SymbolValue[Option[Statement]]
  class variable_modifiers               extends SymbolValue[List[MemberFlag]]
  class method_modifiers                 extends SymbolValue[List[MemberFlag]]
  class non_empty_member_modifiers       extends SymbolValue[List[MemberFlag]]
  class member_modifier                  extends SymbolValue[MemberFlag]

  // Terminals
  class T_STRING                        extends SymbolValue[String]
  class T_INLINE_HTML                   extends SymbolValue[String]
  class T_VARIABLE                      extends SymbolValue[SimpleVariable]

  def _empty_[A]    = rhs() ^^ { () => List[A]() }
  def _emptyOpt_[A] = rhs() ^^ { () => Option.empty[A] }

  def id[T](v: T): T = v
  def inList[T](v: T): List[T] = List(v)
  def inOpt[T](v: T): Option[T] = Some(v)

  def idOf(name: String) = NSIdentifier(NSNone, List(name))
  def simpleIdOf(name: String) = Identifier(name)

  def notyet() { throw new RuntimeException("Not yet implemented") }

  grammar(
    S -> (
        top_statement_list ^^ { (stmts: List[Statement]) => Program(stmts) }
    ),

    top_statement_list -> (
        top_statement_list ~ top_statement ^^ { (stmts: List[Statement], stmt: List[Statement]) => stmts ::: stmt }
      | _empty_
    ),

    top_statement -> (
        statement ^^ inList _
      | function_declaration_statement ^^ inList _
      | class_declaration_statement ^^ inList _
      | T_HALT_COMPILER ~ T_OPEN_BRACES ~ T_CLOSE_BRACES ~ T_SEMICOLON ^^ { () => notyet() }
      | T_NAMESPACE ~ namespace_name ~ T_SEMICOLON ^^ { (nsid: NSIdentifier) => List(NamespaceStart(nsid)) }
      | T_NAMESPACE ~ namespace_name ~ T_OPEN_CURLY_BRACES ~
        top_statement_list ~ T_CLOSE_CURLY_BRACES ^^ { (nsid: NSIdentifier, stmts: List[Statement]) => List(Namespaced(nsid, stmts)) }
      | T_NAMESPACE ~ T_OPEN_CURLY_BRACES ~
        top_statement_list ~ T_CLOSE_CURLY_BRACES ^^ { (stmts: List[Statement]) => List(Namespaced(NSIdentifier(NSResolved, Nil), stmts))  }
      | T_USE ~ use_declarations ~ T_SEMICOLON ^^ id _
      | constant_declaration ~ T_SEMICOLON ^^ id _
    ),

    use_declarations -> (
        use_declarations ~ T_COMMA ~ use_declaration ^^ { (uds: List[Statement], ud: Statement) => uds ::: ud :: Nil }
        | use_declaration ^^ inList _
    ),

    use_declaration -> (
          namespace_name ^^ { (ns: NSIdentifier) => notyet() }
        | namespace_name ~ T_AS ~ T_STRING ^^ { (ns: NSIdentifier, str: String) => notyet() }
        | T_NS_SEPARATOR ~ namespace_name ^^ { (ns: NSIdentifier) => notyet() }
        | T_NS_SEPARATOR ~ namespace_name ~ T_AS ~ T_STRING ^^ { (ns: NSIdentifier, str: String) => notyet() }
    ),

    constant_declaration -> (
          constant_declaration ~ T_COMMA ~ T_STRING ~ T_ASSIGN ~ static_expr ^^ {
            (cd: List[ConstantDecl], name: String, v: Expression) => cd ::: List(ConstantDecl(idOf(name), v))
          }
        | T_CONST ~ T_STRING ~ T_ASSIGN ~ static_expr ^^ {
            (name: String, v: Expression) => List(ConstantDecl(idOf(name), v))
          }
    ),

    inner_statement_list -> (
          inner_statement_list ~ inner_statement ^^ { (sts: List[Statement], st: Statement) => sts ::: st :: Nil }
        | _empty_
    ),

    inner_statement -> (
          statement ^^ id _
        | function_declaration_statement ^^ id _
        | class_declaration_statement ^^ id _
        | T_HALT_COMPILER ~ T_OPEN_BRACES ~ T_CLOSE_BRACES ~ T_SEMICOLON ^^ { () => notyet() }
    ),

    statement -> (
          T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ { (sts: List[Statement]) => Block(sts) }
        | T_IF ~ T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES  ~ statement  ~ elseif_list ~ else_single ^^ {
            (c: Expression, then: Statement, elsifs: List[(Expression, Statement)], elze: Option[Statement]) =>
              ((c, then) :: elsifs).foldLeft(elze) {
                case (elze, (c, then)) => Some(If(c, then, elze))
              }.get
          }
        | T_IF ~ T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES ~ T_COLON  ~ inner_statement_list  ~ new_elseif_list ~ new_else_single ~ T_ENDIF ~ T_SEMICOLON ^^ {
            (c: Expression, then: Statement, elsifs: List[(Expression, Statement)], elze: Option[Statement]) =>
              ((c, then) :: elsifs).foldLeft(elze) {
                case (elze, (c, then)) => Some(If(c, then, elze))
              }.get
          }
        | T_WHILE ~ T_OPEN_BRACES  ~ expr  ~ T_CLOSE_BRACES  ~ while_statement ^^ {
            (c: Expression, body: Statement) => While(c, body)
          }
        | T_DO  ~ statement ~ T_WHILE ~ T_OPEN_BRACES  ~ expr ~ T_CLOSE_BRACES ~ T_SEMICOLON ^^ {
            (body: Statement, c: Expression) => DoWhile(body, c)
          }
        | T_FOR ~
                T_OPEN_BRACES ~
                    for_expr ~
                T_SEMICOLON ~
                    for_expr ~
                T_SEMICOLON ~
                    for_expr ~
                T_CLOSE_BRACES ~
                for_statement ^^ {
            (si: List[Expression], sc: List[Expression], ss: List[Expression], stmt: Statement) =>
              val condition = if (sc.isEmpty) {
                PHPTrue()
              } else {
                sc.reduceLeft{ BooleanAnd(_, _) }
              }
              For(Block(si), condition, Block(ss), stmt)
          }
        | T_SWITCH ~ T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES ~ switch_case_list ^^ {
          (ex: Expression, scs: List[(Option[Expression], Statement)]) =>
          Switch(ex, scs)
        }
        | T_BREAK ~ T_SEMICOLON ^^ {
          () => Break(PHPInteger(1))
        }
        | T_BREAK ~ expr ~ T_SEMICOLON ^^ {
          (ex: Expression) => Break(ex)
        }
        | T_CONTINUE ~ T_SEMICOLON ^^ {
          () => Continue(PHPInteger(1))
        }
        | T_CONTINUE ~ expr ~ T_SEMICOLON ^^ {
          (ex: Expression) => Continue(ex)
        }
        | T_RETURN ~ T_SEMICOLON ^^ {
          () => Return(PHPNull())
        }
        | T_RETURN ~ expr ~ T_SEMICOLON ^^ {
          (ex: Expression) => Return(ex)
        }
        | T_GLOBAL ~ global_var_list ~ T_SEMICOLON ^^ {
          (gls: List[Variable]) =>
            Global(gls)
        }
        | T_STATIC ~ static_var_list ~ T_SEMICOLON ^^ {
          (sts: List[InitVariable]) =>
            Static(sts)
        }
        | T_ECHO ~ echo_expr_list ~ T_SEMICOLON ^^ {
          (ecs: List[Expression]) =>
            Echo(ecs)
        }
        | T_INLINE_HTML ^^ {
          (content: String) =>
            Html(content)
        }
        | expr ~ T_SEMICOLON ^^ {
          (ex: Expression) =>
            ex
        }
        | T_UNSET ~ T_OPEN_BRACES ~ variable_list ~ T_CLOSE_BRACES ~ T_SEMICOLON ^^ {
          (vs: List[Variable]) =>
            Unset(vs)
        }
        | T_FOREACH ~ T_OPEN_BRACES ~ expr ~ T_AS ~
            foreach_variable ~ foreach_optional_arg ~ T_CLOSE_BRACES ~
            foreach_statement ^^ {
            (ex: Expression, f1: (Variable, Boolean), f2: Option[(Variable, Boolean)], stmt: Statement) =>

            f2 match {
              case Some((v2, byref)) =>
                Foreach(ex, v2, byref, Some(f1._1), f1._2, stmt)
              case None =>
                Foreach(ex, f1._1, f1._2, None, false, stmt)
            }
        }
        | T_DECLARE ~ T_OPEN_BRACES ~ declare_list ~ T_CLOSE_BRACES ~ declare_statement ^^ {
          (dl: List[String], stmt: Statement) => Void()
        }
        | T_SEMICOLON /* empty statement */ ^^ {
          () => Void()
        }
        | T_TRY ~ T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ~
            T_CATCH ~ T_OPEN_BRACES ~
            fully_qualified_class_name ~
            T_VARIABLE ~ T_CLOSE_BRACES ~
            T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ~
            additional_catches ^^ {

          (st: Block, cl: StaticClassRef, v: SimpleVariable, clbody: Block, catches: List[Catch]) =>
            Try(st, Catch(cl, v, clbody) :: catches)
        }
        | T_THROW ~ expr ~ T_SEMICOLON ^^ {
          (ex: Expression) =>
            Throw(ex)
        }
        | T_GOTO ~ T_STRING ~ T_SEMICOLON ^^ {
          (s: String) =>
            Goto(Label(Identifier(s)))
        }
        | T_STRING ~ T_COLON ^^ {
          (s: String) =>
            LabelDecl(Identifier(s))
        }
    ),

    additional_catches -> (
          non_empty_additional_catches ^^ id _
        | _empty_
    ),

    non_empty_additional_catches -> (
          additional_catch ^^ inList _
        | non_empty_additional_catches ~ additional_catch ^^ {
          (ls: List[Catch], c: Catch) =>
            ls ::: List(c)
        }
    ),

    additional_catch -> (
        T_CATCH ~ T_OPEN_BRACES ~ fully_qualified_class_name ~ T_VARIABLE ~ T_CLOSE_BRACES ~ T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ {
          (cl: StaticClassRef, v: SimpleVariable, b: Block) =>
            Catch(cl, v, b)
        }
    ),

    variable_list -> (
          variable ^^ inList _
        | variable_list ~ T_COMMA ~ variable ^^ {
          (ls: List[Variable], v: Variable) =>
            ls ::: List(v)
        }
    ),

    is_reference -> (
          rhs() ^^ { () => false }
        | T_BITWISE_AND ^^ { () => true }
    ),

    function_declaration_statement -> (
          T_FUNCTION ~ is_reference ~ T_STRING ~
                T_OPEN_BRACES ~ parameter_list ~ T_CLOSE_BRACES ~ T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ {

            (byRef: Boolean, name: String, params: List[ArgumentDecl], body: Block) =>
              FunctionDecl(idOf(name), params, byRef, body)
          }
    ),

    class_declaration_statement -> (
          class_entry_type ~ T_STRING ~ extends_from ~
                implements_list ~
                T_OPEN_CURLY_BRACES ~
                    class_statement_list ~
                T_CLOSE_CURLY_BRACES ^^ {
                  (flag: ClassFlag, name: String, exts: Option[StaticClassRef], ifaces: List[StaticClassRef], stmts: (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])) =>
            val (methods, static_props, props, consts) = stmts
            ClassDecl(idOf(name), flag, exts, ifaces, methods, static_props, props, consts)
          }
        | T_INTERFACE ~ T_STRING ~
                interface_extends_list ~
                T_OPEN_CURLY_BRACES ~
                    class_statement_list ~
                T_CLOSE_CURLY_BRACES ^^ {
                  (flag: ClassFlag, name: String, ifaces: List[StaticClassRef], stmts: (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])) =>
            val (methods, static_props, props, consts) = stmts
            InterfaceDecl(idOf(name), ifaces, methods, consts)
          }
    ),

    class_entry_type -> (
          T_CLASS ^^ { () => CFNormal }
        | T_ABSTRACT ~ T_CLASS ^^ { () => CFAbstract }
        | T_FINAL ~ T_CLASS ^^ { () => CFFinal }
    ),

    extends_from -> (
          _emptyOpt_ /* empty */
        | T_EXTENDS ~ fully_qualified_class_name ^^ {
          (r: StaticClassRef) => Some(r)
        }
    ),

    interface_extends_list -> (
          _empty_
        | T_EXTENDS ~ interface_list ^^ id _
    ),

    implements_list -> (
          _empty_
        | T_IMPLEMENTS ~ interface_list ^^ id _
    ),

    interface_list -> (
          fully_qualified_class_name ^^ inList _
        | interface_list ~ T_COMMA ~ fully_qualified_class_name ^^ {
          (ls: List[StaticClassRef], r: StaticClassRef) =>
            ls ::: List(r)
        }
    ),

    foreach_optional_arg -> (
          _emptyOpt_
        | T_DOUBLE_ARROW ~ foreach_variable ^^ inOpt _
    ),

    foreach_variable -> (
          variable ^^ { (v: Variable) => (v, false) }
        | T_BITWISE_AND ~ variable ^^ { (v: Variable) => (v, true) }
    ),

    for_statement -> (
          statement ^^ id _
        | T_COLON ~ inner_statement_list ~ T_ENDFOR ~ T_SEMICOLON ^^ id _
    ),

    foreach_statement -> (
          statement ^^ id _
        | T_COLON ~ inner_statement_list ~ T_ENDFOREACH ~ T_SEMICOLON ^^ id _
    ),

    declare_statement -> (
          statement ^^ id _
        | T_COLON ~ inner_statement_list ~ T_ENDDECLARE ~ T_SEMICOLON ^^ id _
    ),

    /* Todo */
    declare_list -> (
          T_STRING ~ T_ASSIGN ~ static_expr ^^ {
          (name: String, v: Expression) => Nil
        }
        | declare_list ~ T_COMMA ~ T_STRING ~ T_ASSIGN ~ static_expr ^^ {
          (ls: List[String], name: String, v: Expression) => Nil
        }
    ),

    switch_case_list -> (
          T_OPEN_CURLY_BRACES ~ case_list ~ T_CLOSE_CURLY_BRACES ^^ id _
        | T_OPEN_CURLY_BRACES ~ T_SEMICOLON ~ case_list ~ T_CLOSE_CURLY_BRACES ^^ id _
        | T_COLON ~ case_list ~ T_ENDSWITCH ~ T_SEMICOLON ^^ id _
        | T_COLON ~ T_SEMICOLON ~ case_list ~ T_ENDSWITCH ~ T_SEMICOLON ^^ id _
    ),

    case_list -> (
          _empty_
        | case_list ~ T_CASE ~ expr ~ case_separator ~ inner_statement_list ^^ {
          (ls: List[(Option[Expression], Statement)], ex: Expression, st: Statement) =>
            ls ::: List((Some(ex), st))
        }
        | case_list ~ T_DEFAULT ~ case_separator ~ inner_statement_list ^^ {
          (ls: List[(Option[Expression], Statement)], st: Statement) =>
            ls ::: List((None, st))
        }
    ),

    case_separator -> (
          T_COLON ^^ { () => None }
        | T_SEMICOLON ^^ { () => None }
    ),

    while_statement -> (
          statement ^^ id _
        | T_COLON ~ inner_statement_list ~ T_ENDWHILE ~ T_SEMICOLON ^^ id _
    ),

    elseif_list -> (
          _empty_
        | elseif_list ~ T_ELSEIF ~ T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES ~ statement ^^ {
          (ls: List[(Expression, Statement)], ex: Expression, st: Statement) =>
            ls ::: List((ex, st))
        }
    ),

    new_elseif_list -> (
          _empty_
        | new_elseif_list ~ T_ELSEIF ~ T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES ~ T_COLON ~ inner_statement_list ^^ {
          (ls: List[(Expression, Statement)], ex: Expression, st: Statement) =>
            ls ::: List((ex, st))
        }
    ),

    else_single -> (
          _emptyOpt_ /* empty */
        | T_ELSE ~ statement ^^ inOpt _
    ),

    new_else_single -> (
          _emptyOpt_ /* empty */
        | T_ELSE ~ T_COLON ~ inner_statement_list ^^ inOpt _
    ),

    parameter_list -> (
          non_empty_parameter_list ^^ id _
        | _empty_ /* empty */
    ),

    non_empty_parameter_list -> (
          optional_class_type ~ T_VARIABLE ^^ {
            (hint: Option[TypeHint], v: SimpleVariable) =>
              List(ArgumentDecl(v, hint, None, false))
          }
        | optional_class_type ~ T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (hint: Option[TypeHint], v: SimpleVariable, ex: Expression) =>
              List(ArgumentDecl(v, hint, Some(ex), false))
          }
        | optional_class_type ~ T_BITWISE_AND ~ T_VARIABLE ^^ {
            (hint: Option[TypeHint], v: SimpleVariable) =>
              List(ArgumentDecl(v, hint, None, true))
          }
        | optional_class_type ~ T_BITWISE_AND ~ T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (hint: Option[TypeHint], v: SimpleVariable, ex: Expression) =>
              List(ArgumentDecl(v, hint, Some(ex), true))
          }
        | non_empty_parameter_list ~ T_COMMA ~ optional_class_type ~ T_VARIABLE ^^ {
            (ls: List[ArgumentDecl], hint: Option[TypeHint], v: SimpleVariable) =>
              ls ::: List(ArgumentDecl(v, hint, None, false))
          }
        | non_empty_parameter_list ~ T_COMMA ~ optional_class_type ~ T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (ls: List[ArgumentDecl], hint: Option[TypeHint], v: SimpleVariable, ex: Expression) =>
              ls ::: List(ArgumentDecl(v, hint, Some(ex), false))
          }
        | non_empty_parameter_list ~ T_COMMA ~ optional_class_type ~ T_BITWISE_AND ~ T_VARIABLE ^^ {
            (ls: List[ArgumentDecl], hint: Option[TypeHint], v: SimpleVariable) =>
              ls ::: List(ArgumentDecl(v, hint, None, true))
          }
        | non_empty_parameter_list ~ T_COMMA ~ optional_class_type ~ T_BITWISE_AND ~ T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (ls: List[ArgumentDecl], hint: Option[TypeHint], v: SimpleVariable, ex: Expression) =>
              ls ::: List(ArgumentDecl(v, hint, Some(ex), true))
          }
    ),

    optional_class_type -> (
          _emptyOpt_ /* empty */
        | fully_qualified_class_name ^^ {
          (r: StaticClassRef) => Some(THObject(r))
        }
        | T_ARRAY ^^ {
          () => Some(THArray)
        }
    ),

    function_call_parameter_list -> (
          non_empty_function_call_parameter_list ^^ id _
        | _empty_ /* empty */
    ),

    non_empty_function_call_parameter_list -> (
          expr ^^ {
            (ex: Expression) =>
              List(CallArg(ex, false))
          }
        | T_BITWISE_AND ~ variable ^^ {
            (ex: Expression) =>
              List(CallArg(ex, true))
        }
        | non_empty_function_call_parameter_list ~ T_COMMA ~ expr ^^ {
            (ls: List[CallArg], ex: Expression) =>
              ls ::: List(CallArg(ex, false))
        }
        | non_empty_function_call_parameter_list ~ T_COMMA ~ T_BITWISE_AND ~ variable ^^ {
            (ls: List[CallArg], ex: Expression) =>
              ls ::: List(CallArg(ex, true))
        }
    ),

    global_var_list -> (
          global_var_list ~ T_COMMA ~ global_var ^^ {
            (gls: List[Variable], v: Variable) =>
              gls ::: List(v)
          }
        | global_var ^^ inList _
    ),

    global_var -> (
          T_VARIABLE ^^ id _
        | T_DOLLAR ~ variable ^^ {
            (v: Variable) => VariableVariable(v)
          }
        | T_DOLLAR ~ T_OPEN_CURLY_BRACES ~ expr ~ T_CLOSE_CURLY_BRACES ^^ {
            (ex: Expression) => VariableVariable(ex)
          }
    ),

    static_var_list -> (
          static_var_list ~ T_COMMA ~ T_VARIABLE ^^ {
            (ls: List[InitVariable], v: SimpleVariable) =>
              ls ::: List(InitVariable(v, None))
          }
        | static_var_list ~ T_COMMA ~ T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (ls: List[InitVariable], v: SimpleVariable, ex: Expression) =>
              ls ::: List(InitVariable(v, Some(ex)))
          }
        | T_VARIABLE ^^ {
            (v: SimpleVariable) =>
              List(InitVariable(v, None))
          }
        | T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (v: SimpleVariable, ex: Expression) =>
              List(InitVariable(v, Some(ex)))
          }
    ),

    class_statement_list -> (
          class_statement_list ~ class_statement ^^ {
            (ls: (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl]), cst: (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])) =>
            (ls._1 ::: cst._1, ls._2 ::: cst._2, ls._3 ::: cst._3, ls._4 ::: cst._4)
          }
        | rhs() ^^ { () => (Nil, Nil, Nil, Nil) }
    ),

    class_statement -> (
          variable_modifiers ~ class_variable_declaration ~ T_SEMICOLON ^^ {
            (flags: List[MemberFlag], prop: PropertyDecl) =>
              if (flags contains MFStatic) {
                (Nil, List(prop), Nil, Nil)
              } else {
                (Nil, Nil, List(prop), Nil)
              }
          }
        | class_constant_declaration ~ T_SEMICOLON ^^ {
            (ccd: ClassConstantDecl) =>
              (Nil, Nil, Nil, List(ccd))
          }
        | method_modifiers ~ T_FUNCTION ~ is_reference ~ T_STRING ~ T_OPEN_BRACES ~
                parameter_list ~ T_CLOSE_BRACES ~ method_body ^^ {
            (flags: List[MemberFlag], isRef: Boolean, name: String, params: List[ArgumentDecl], body: Option[Statement]) =>
              (List(MethodDecl(simpleIdOf(name), flags, params, isRef, body)), Nil, Nil, Nil)
          }
    ),

    method_body -> (
          T_SEMICOLON ^^ { () => None } /* abstract method */
        | T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ inOpt _
    ),

    variable_modifiers -> (
          non_empty_member_modifiers ^^ id _
        | T_VAR ^^ { () => List(MFPublic) }
    ),

    method_modifiers -> (
          _empty_ /* empty */
        | non_empty_member_modifiers ^^ id _
    ),

    non_empty_member_modifiers -> (
          member_modifier ^^ inList _
        | non_empty_member_modifiers ~ member_modifier ^^ {
          (ls :List[MemberFlag], f: MemberFlag) => ls ::: List(f)
        }
    ),

    member_modifier -> (
          T_PUBLIC ^^     { () => MFPublic }
        | T_PROTECTED ^^  { () => MFProtected }
        | T_PRIVATE ^^    { () => MFPrivate }
        | T_STATIC ^^     { () => MFStatic }
        | T_ABSTRACT ^^   { () => MFAbstract }
        | T_FINAL ^^      { () => MFFinal }
    ),

    class_variable_declaration -> (
          class_variable_declaration T_COMMA T_VARIABLE
        | class_variable_declaration T_COMMA T_VARIABLE T_ASSIGN static_expr
        | T_VARIABLE
        | T_VARIABLE T_ASSIGN static_expr
    ),

    class_constant_declaration -> (
          class_constant_declaration T_COMMA T_STRING T_ASSIGN static_expr
        | T_CONST T_STRING T_ASSIGN static_expr
    ),

    echo_expr_list -> (
          echo_expr_list T_COMMA expr
        | expr
    ),

    for_expr -> (
          /* empty */
        | non_empty_for_expr
    ),

    non_empty_for_expr -> (
          non_empty_for_expr T_COMMA	 expr
        | expr
    ),

    lexical_vars -> (
          /* empty */
        | T_USE T_OPEN_BRACES lexical_var_list T_CLOSE_BRACES
    ),

    lexical_var_list -> (
          lexical_var_list T_COMMA T_VARIABLE
        | lexical_var_list T_COMMA T_BITWISE_AND T_VARIABLE
        | T_VARIABLE
        | T_BITWISE_AND T_VARIABLE
    ),

    function_call -> (
          namespace_name T_OPEN_BRACES 
                    function_call_parameter_list
                    T_CLOSE_BRACES
        | T_NAMESPACE T_NS_SEPARATOR namespace_name T_OPEN_BRACES 
                    function_call_parameter_list
                    T_CLOSE_BRACES
        | T_NS_SEPARATOR namespace_name T_OPEN_BRACES 
                    function_call_parameter_list
                    T_CLOSE_BRACES
        | class_name T_DOUBLE_COLON T_STRING T_OPEN_BRACES 
                function_call_parameter_list
                T_CLOSE_BRACES
        | class_name T_DOUBLE_COLON variable_without_objects T_OPEN_BRACES 
                function_call_parameter_list
                T_CLOSE_BRACES
        | reference_variable T_DOUBLE_COLON T_STRING T_OPEN_BRACES 
                function_call_parameter_list
                T_CLOSE_BRACES
        | reference_variable T_DOUBLE_COLON variable_without_objects T_OPEN_BRACES 
                function_call_parameter_list
                T_CLOSE_BRACES
        | variable_without_objects  T_OPEN_BRACES 
                function_call_parameter_list T_CLOSE_BRACES
    ),

    class_name -> (
          T_STATIC
        | namespace_name
        | T_NAMESPACE T_NS_SEPARATOR namespace_name
        | T_NS_SEPARATOR namespace_name
    ),

    fully_qualified_class_name -> (
          namespace_name
        | T_NAMESPACE T_NS_SEPARATOR namespace_name
        | T_NS_SEPARATOR namespace_name
    ),

    class_name_reference -> (
          class_name
        | dynamic_class_name_reference
    ),

    dynamic_class_name_reference -> (
          base_variable T_OBJECT_OPERATOR 
                object_property  dynamic_class_name_variable_properties
        | base_variable
    ),

    dynamic_class_name_variable_properties -> (
          dynamic_class_name_variable_properties T_OBJECT_OPERATOR object_property
        | /* empty */
    ),

    exit_expr -> (
          /* empty */
        | T_OPEN_BRACES T_CLOSE_BRACES
        | T_OPEN_BRACES expr T_CLOSE_BRACES
    ),

    backticks_expr -> (
          /* empty */
        | T_STRING
        | T_ENCAPSED_AND_WHITESPACE
        | encaps_list
    ),

    ctor_arguments -> (
          /* empty */
        | T_OPEN_BRACES function_call_parameter_list T_CLOSE_BRACES
    ),

    common_scalar -> (
          T_LNUMBER
        | T_DNUMBER
        | T_CONSTANT_ENCAPSED_STRING
        | T_LINE
        | T_FILE
        | T_DIR
        | T_CLASS_C
        | T_METHOD_C
        | T_FUNC_C
        | T_NS_C
        | T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC
        | T_START_HEREDOC T_CONSTANT_ENCAPSED_STRING T_END_HEREDOC
        | T_START_HEREDOC T_END_HEREDOC
    ),

    static_expr -> (
          common_scalar
        | namespace_name
        | T_NAMESPACE T_NS_SEPARATOR namespace_name
        | T_NS_SEPARATOR namespace_name
        | T_PLUS static_expr
        | T_MINUS static_expr
        | T_ARRAY T_OPEN_BRACES static_array_pair_list T_CLOSE_BRACES
        | static_class_constant
    ),

    static_class_constant -> (
          class_name T_DOUBLE_COLON T_STRING
    ),

    scalar -> (
          class_constant
        | namespace_name
        | T_NAMESPACE T_NS_SEPARATOR namespace_name
        | T_NS_SEPARATOR namespace_name
        | common_scalar
        | T_DOUBLE_QUOTE encaps_list T_DOUBLE_QUOTE
        | T_DOUBLE_QUOTE T_ENCAPSED_AND_WHITESPACE T_DOUBLE_QUOTE
        | T_START_HEREDOC encaps_list T_END_HEREDOC
    ),

    static_array_pair_list -> (
          /* empty */
        | non_empty_static_array_pair_list possible_comma
    ),

    possible_comma -> (
          /* empty */
        | T_COMMA
    ),

    non_empty_static_array_pair_list -> (
          non_empty_static_array_pair_list T_COMMA static_expr T_DOUBLE_ARROW static_expr
        | non_empty_static_array_pair_list T_COMMA static_expr
        | static_expr T_DOUBLE_ARROW static_expr
        | static_expr
    ),

    expr -> (
          variable
        | T_LIST T_OPEN_BRACES  assignment_list T_CLOSE_BRACES T_ASSIGN expr
        | variable T_ASSIGN expr
        | variable T_ASSIGN T_BITWISE_AND variable
        | variable T_ASSIGN T_BITWISE_AND T_NEW class_name_reference  ctor_arguments
        | T_NEW class_name_reference  ctor_arguments
        | T_CLONE expr
        | variable T_PLUS_EQUAL expr
        | variable T_MINUS_EQUAL expr
        | variable T_MUL_EQUAL expr
        | variable T_DIV_EQUAL expr
        | variable T_CONCAT_EQUAL expr
        | variable T_MOD_EQUAL expr
        | variable T_AND_EQUAL expr
        | variable T_OR_EQUAL expr
        | variable T_XOR_EQUAL expr
        | variable T_SL_EQUAL expr
        | variable T_SR_EQUAL expr
        | variable T_INC
        | T_INC variable
        | variable T_DEC
        | T_DEC variable
        | expr T_BOOLEAN_OR  expr
        | expr T_BOOLEAN_AND  expr
        | expr T_LOGICAL_OR  expr
        | expr T_LOGICAL_AND  expr
        | expr T_LOGICAL_XOR expr
        | expr T_BITWISE_OR expr
        | expr T_BITWISE_AND expr
        | expr T_BITWISE_XOR expr
        | expr T_POINT expr
        | expr T_PLUS expr
        | expr T_MINUS expr
        | expr T_MULT expr
        | expr T_DIV expr
        | expr T_MODULO expr
        | expr T_SL expr
        | expr T_SR expr
        | T_PLUS expr
        | T_MINUS expr
        | T_NOT expr
        | T_BITWISE_NOT expr
        | expr T_IS_IDENTICAL expr
        | expr T_IS_NOT_IDENTICAL expr
        | expr T_IS_EQUAL expr
        | expr T_IS_NOT_EQUAL expr
        | expr T_IS_SMALLER expr
        | expr T_IS_SMALLER_OR_EQUAL expr
        | expr T_IS_GREATER expr
        | expr T_IS_GREATER_OR_EQUAL expr
        | expr T_INSTANCEOF class_name_reference
        | T_OPEN_BRACES expr T_CLOSE_BRACES
        | expr T_QUESTION 
            expr T_COLON 
            expr
        | expr T_QUESTION T_COLON 
            expr
        | internal_functions_in_yacc
        | T_INT_CAST expr
        | T_DOUBLE_CAST expr
        | T_STRING_CAST expr
        | T_ARRAY_CAST expr
        | T_OBJECT_CAST expr
        | T_BOOL_CAST expr
        | T_UNSET_CAST expr
        | T_EXIT exit_expr
        | T_AT  expr
        | scalar
        | T_ARRAY T_OPEN_BRACES array_pair_list T_CLOSE_BRACES
        | T_BACKTICK backticks_expr T_BACKTICK
        | T_PRINT expr
        | T_FUNCTION is_reference T_OPEN_BRACES 
                parameter_list T_CLOSE_BRACES lexical_vars T_OPEN_CURLY_BRACES inner_statement_list T_CLOSE_CURLY_BRACES
    ),

    variable -> (
          base_variable_with_function_calls T_OBJECT_OPERATOR 
                object_property  method_or_not variable_properties
        | base_variable_with_function_calls
    ),

    variable_properties -> (
          variable_properties variable_property
        | /* empty */
    ),

    variable_property -> (
          T_OBJECT_OPERATOR object_property  method_or_not
    ),

    method_or_not -> (
          T_OPEN_BRACES 
                    function_call_parameter_list T_CLOSE_BRACES
        | /* empty */
    ),

    variable_without_objects -> (
          reference_variable
        | simple_indirect_reference reference_variable
    ),

    static_member -> (
          class_name T_DOUBLE_COLON variable_without_objects
        | reference_variable T_DOUBLE_COLON variable_without_objects
    ),

    base_variable_with_function_calls -> (
          base_variable
        | function_call
    ),

    base_variable -> (
          reference_variable
        | simple_indirect_reference reference_variable
        | static_member
    ),

    reference_variable -> (
          reference_variable T_OPEN_RECT_BRACES dim_offset T_CLOSE_RECT_BRACES
        | reference_variable T_OPEN_CURLY_BRACES expr T_CLOSE_CURLY_BRACES
        | compound_variable
    ),

    compound_variable -> (
          T_VARIABLE
        | T_DOLLAR T_OPEN_CURLY_BRACES expr T_CLOSE_CURLY_BRACES
    ),

    dim_offset -> (
          /* empty */
        | expr
    ),

    object_property -> (
          object_dim_list
        | variable_without_objects
    ),

    object_dim_list -> (
          object_dim_list T_OPEN_RECT_BRACES dim_offset T_CLOSE_RECT_BRACES
        | object_dim_list T_OPEN_CURLY_BRACES expr T_CLOSE_CURLY_BRACES
        | variable_name
    ),

    variable_name -> (
          T_STRING
        | T_OPEN_CURLY_BRACES expr T_CLOSE_CURLY_BRACES
    ),

    simple_indirect_reference -> (
          T_DOLLAR
        | simple_indirect_reference T_DOLLAR
    ),

    assignment_list -> (
          assignment_list T_COMMA assignment_list_element
        | assignment_list_element
    ),

    assignment_list_element -> (
          variable
        | T_LIST T_OPEN_BRACES  assignment_list T_CLOSE_BRACES
        | /* empty */
    ),

    array_pair_list -> (
          /* empty */
        | non_empty_array_pair_list possible_comma
    ),

    non_empty_array_pair_list -> (
          non_empty_array_pair_list T_COMMA expr T_DOUBLE_ARROW expr
        | non_empty_array_pair_list T_COMMA expr
        | expr T_DOUBLE_ARROW expr
        | expr
        | non_empty_array_pair_list T_COMMA expr T_DOUBLE_ARROW T_BITWISE_AND variable
        | non_empty_array_pair_list T_COMMA T_BITWISE_AND variable
        | expr T_DOUBLE_ARROW T_BITWISE_AND variable
        | T_BITWISE_AND variable
    ),

    encaps_list -> (
          encaps_list encaps_var
        | encaps_list T_ENCAPSED_AND_WHITESPACE
        | encaps_var
        | T_ENCAPSED_AND_WHITESPACE encaps_var
    ),


    encaps_var -> (
          T_VARIABLE
        | T_VARIABLE T_OPEN_RECT_BRACES encaps_var_offset T_CLOSE_RECT_BRACES
        | T_VARIABLE T_OBJECT_OPERATOR T_STRING
        | T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME T_CLOSE_CURLY_BRACES
        | T_DOLLAR_OPEN_CURLY_BRACES expr T_CLOSE_CURLY_BRACES
        | T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME T_OPEN_RECT_BRACES expr T_CLOSE_RECT_BRACES T_CLOSE_CURLY_BRACES
        | T_CURLY_OPEN variable T_CLOSE_CURLY_BRACES
    ),

    encaps_var_offset -> (
          T_STRING
        | T_NUM_STRING
        | T_VARIABLE
    ),

    internal_functions_in_yacc -> (
          T_ISSET T_OPEN_BRACES isset_variables T_CLOSE_BRACES
        | T_EMPTY T_OPEN_BRACES variable T_CLOSE_BRACES
        | T_INCLUDE expr
        | T_INCLUDE_ONCE expr
        | T_EVAL T_OPEN_BRACES expr T_CLOSE_BRACES
        | T_REQUIRE expr
        | T_REQUIRE_ONCE expr
    ),

    isset_variables -> (
          variable
        | isset_variables T_COMMA  variable
    ),

    class_constant -> (
          class_name T_DOUBLE_COLON T_STRING
        | reference_variable T_DOUBLE_COLON T_STRING
    )
  )
}
