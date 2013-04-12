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

  case class IntWrapper(v: Int)

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
  class additional_catch                extends SymbolValue[Catch]
  class additional_catches              extends SymbolValue[List[Catch]]
  class array_pair_list                 extends SymbolValue[List[(Option[Expression], Expression, Boolean)]]
  class assignment_list                 extends SymbolValue[List[Option[Variable]]]
  class assignment_list_element         extends SymbolValue[Option[Variable]]
  class backticks_expr                  extends SymbolValue[Option[Expression]]
  class base_variable                   extends SymbolValue[Variable]
  class base_variable_with_function_calls extends SymbolValue[Expression]
  class case_list                       extends SymbolValue[List[(Option[Expression], Statement)]]
  class case_separator                  extends SymbolValue[Option[Nothing]]
  class class_constant                  extends SymbolValue[ClassConstant]
  class class_constant_declaration      extends SymbolValue[List[ClassConstantDecl]]
  class class_declaration_statement     extends SymbolValue[Statement]
  class class_entry_type                extends SymbolValue[ClassFlag]
  class class_name                      extends SymbolValue[ClassRef]
  class class_name_reference            extends SymbolValue[ClassRef]
  class class_statement                 extends SymbolValue[(List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])]
  class class_statement_list            extends SymbolValue[(List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])]
  class class_variable_declaration      extends SymbolValue[List[PropertyDecl]]
  class common_scalar                   extends SymbolValue[Expression]
  class compound_variable               extends SymbolValue[Variable]
  class constant_declaration            extends SymbolValue[List[ConstantDecl]]
  class ctor_arguments                  extends SymbolValue[List[CallArg]]
  class declare_list                    extends SymbolValue[List[String]]
  class declare_statement               extends SymbolValue[Statement]
  class dim_offset                      extends SymbolValue[Option[Expression]]
  class dynamic_class_name_reference    extends SymbolValue[ClassRef]
  class dynamic_class_name_variable_properties extends SymbolValue[List[ObjectAccess]]
  class echo_expr_list                  extends SymbolValue[List[Expression]]
  class else_single                     extends SymbolValue[Option[Statement]]
  class elseif_list                     extends SymbolValue[List[(Expression, Statement)]]
  class encaps_list                     extends SymbolValue[Expression]
  class encaps_var                      extends SymbolValue[Variable]
  class encaps_var_offset               extends SymbolValue[Expression]
  class exit_expr                       extends SymbolValue[Option[Expression]]
  class expr                            extends SymbolValue[Expression]
  class extends_from                    extends SymbolValue[Option[StaticClassRef]]
  class for_expr                        extends SymbolValue[List[Expression]]
  class for_statement                   extends SymbolValue[Statement]
  class foreach_optional_arg            extends SymbolValue[Option[(Variable, Boolean)]]
  class foreach_statement               extends SymbolValue[Statement]
  class foreach_variable                extends SymbolValue[(Variable, Boolean)]
  class fully_qualified_class_name      extends SymbolValue[StaticClassRef]
  class function_call                   extends SymbolValue[Expression]
  class function_call_parameter_list    extends SymbolValue[List[CallArg]]
  class function_declaration_statement  extends SymbolValue[FunctionDecl]
  class global_var                      extends SymbolValue[Variable]
  class global_var_list                 extends SymbolValue[List[Variable]]
  class implements_list                 extends SymbolValue[List[StaticClassRef]]
  class inner_statement                 extends SymbolValue[Statement]
  class inner_statement_list            extends SymbolValue[List[Statement]]
  class interface_extends_list          extends SymbolValue[List[StaticClassRef]]
  class interface_list                  extends SymbolValue[List[StaticClassRef]]
  class internal_functions_in_yacc      extends SymbolValue[Expression]
  class is_reference                    extends SymbolValue[Option[Boolean]]
  class isset_variables                 extends SymbolValue[List[Variable]]
  class lexical_var_list                extends SymbolValue[List[ArgumentDecl]]
  class lexical_vars                    extends SymbolValue[List[ArgumentDecl]]
  class member_modifier                 extends SymbolValue[MemberFlag]
  class method_body                     extends SymbolValue[Option[Statement]]
  class method_modifiers                extends SymbolValue[List[MemberFlag]]
  class method_or_not                   extends SymbolValue[Option[List[ObjectAccess]]]
  class namespace_name                  extends SymbolValue[NSIdentifier]
  class new_else_single                 extends SymbolValue[Option[Statement]]
  class new_elseif_list                 extends SymbolValue[List[(Expression, Statement)]]
  class non_empty_additional_catches    extends SymbolValue[List[Catch]]
  class non_empty_array_pair_list       extends SymbolValue[List[(Option[Expression], Expression, Boolean)]]
  class non_empty_for_expr              extends SymbolValue[List[Expression]]
  class non_empty_function_call_parameter_list extends SymbolValue[List[CallArg]]
  class non_empty_member_modifiers      extends SymbolValue[List[MemberFlag]]
  class non_empty_parameter_list        extends SymbolValue[List[ArgumentDecl]]
  class non_empty_static_array_pair_list extends SymbolValue[List[(Option[Expression], Expression, Boolean)]]
  class object_dim_list                 extends SymbolValue[ObjectAccess]
  class object_property                 extends SymbolValue[ObjectAccess]
  class optional_class_type             extends SymbolValue[Option[TypeHint]]
  class parameter_list                  extends SymbolValue[List[ArgumentDecl]]
  class possible_comma                  extends SymbolValue[Option[Boolean]]
  class reference_variable              extends SymbolValue[Variable]
  class scalar                          extends SymbolValue[Expression]
  class simple_indirect_reference       extends SymbolValue[IntWrapper]
  class statement                       extends SymbolValue[Statement]
  class static_array_pair_list          extends SymbolValue[List[(Option[Expression], Expression, Boolean)]]
  class static_class_constant           extends SymbolValue[ClassConstant]
  class static_expr                     extends SymbolValue[Expression]
  class static_member                   extends SymbolValue[ClassProperty]
  class static_var_list                 extends SymbolValue[List[InitVariable]]
  class switch_case_list                extends SymbolValue[List[(Option[Expression], Statement)]]
  class top_statement                   extends SymbolValue[List[Statement]]
  class top_statement_list              extends SymbolValue[List[Statement]]
  class use_declaration                 extends SymbolValue[Statement]
  class use_declarations                extends SymbolValue[List[Statement]]
  class variable                        extends SymbolValue[Expression]
  class variable_list                   extends SymbolValue[List[Variable]]
  class variable_modifiers              extends SymbolValue[List[MemberFlag]]
  class variable_name                   extends SymbolValue[OAScalar]
  class variable_properties             extends SymbolValue[List[ObjectAccess]]
  class variable_property               extends SymbolValue[ObjectAccess]
  class variable_without_objects        extends SymbolValue[Variable]
  class while_statement                 extends SymbolValue[Statement]

  // Terminals
  class T_STRING                        extends SymbolValue[String]
  class T_NUM_STRING                    extends SymbolValue[String]
  class T_STRING_VARNAME                extends SymbolValue[String]
  class T_LNUMBER                       extends SymbolValue[PHPInteger]
  class T_DNUMBER                       extends SymbolValue[PHPFloat]
  class T_CONSTANT_ENCAPSED_STRING      extends SymbolValue[PHPString]
  class T_ENCAPSED_AND_WHITESPACE       extends SymbolValue[PHPString]
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

  def deriveOAList(baseex: Expression, oaList: List[ObjectAccess]): Expression = {
    var ex = baseex
    for(oa <- oaList) {
        oa match {
            case OAIdentifier(id) => ex = ObjectProperty(ex, id).setPosBetween(ex, oa)
            case OAArray(array, indexes) =>
                array match {
                    case id @ OAIdentifier(name) =>
                        ex = ObjectProperty(ex, name).setPosBetween(ex, oa)
                    case _ =>
                }
                for(id <- indexes) id match {
                    case Some(i) => ex = ArrayEntry(ex, i).setPosBetween(ex, oa) // TODO: Fix precision
                    case None => ex = NextArrayEntry(ex).setPosBetween(ex, oa)
                }
            case OAExpression(exp) => ex = DynamicObjectProperty(ex, exp).setPosBetween(ex, oa)
            case OAMethod(name, args) => name match {
                case OAIdentifier(id) => ex = MethodCall(ex, StaticMethodRef(id).setPos(id), args).setPosBetween(ex, oa)
                case OAExpression(e)  => ex = MethodCall(ex, DynamicMethodRef(e).setPos(e), args).setPosBetween(ex, oa)
                case OAArray(array, indexes) =>  {
                    for(id <- indexes) id match {
                        case Some(i) => ex = ArrayEntry(ex, i).setPosBetween(ex, name) // TODO: Fix precision
                        case None => ex = NextArrayEntry(ex).setPosBetween(ex, name)
                    }

                    ex = FunctionCall(DynamicFunctionRef(ex).setPos(ex), args).setPosBetween(ex, oa)
                }
            }
        }
    }
    ex
  }

  def staticConstant(nsid: NSIdentifier): Expression = {
    val res: Expression = if (nsid.parts.length == 1) {
        if (nsid.parts.head.toLowerCase.equals("true")){
          PHPTrue()
        } else if (nsid.parts.head.toLowerCase.equals("false")) {
          PHPFalse()
        } else if (nsid.parts.head.toLowerCase.equals("null")) {
          PHPNull()
        } else {
          Constant(nsid)
        }
    } else {
        Constant(nsid)
    }

    res
  }

  def maybeMethod(oa: ObjectAccess, isMethod: Option[List[CallArg]]): ObjectAccess = {
    isMethod match {
      case Some(args) =>
        OAMethod(oa, args)
      case None => oa
    }
  }

  def writeable(ex: Expression): Variable = ex match {
    case v: Variable => v
    case _ => sys.error("Cannot write to non-variable")
  }

  grammar(
    S -> (
        top_statement_list ^^ { (stmts: List[Statement]) => Program(stmts) }
    ),

    top_statement_list -> (
        top_statement_list ~ top_statement ^^ { (stmts: List[Statement], stmt: List[Statement]) => stmts ::: stmt }
      | _empty_
    ),

    top_statement -> (
        statement ^^ inList[Statement] _
      | function_declaration_statement ^^ inList[FunctionDecl] _
      | class_declaration_statement ^^ inList[Statement] _
      | T_HALT_COMPILER ~ T_OPEN_BRACES ~ T_CLOSE_BRACES ~ T_SEMICOLON ^^ { () => notyet() }
      | T_NAMESPACE ~ namespace_name ~ T_SEMICOLON ^^ { (nsid: NSIdentifier) => List(NamespaceStart(nsid)) }
      | T_NAMESPACE ~ namespace_name ~ T_OPEN_CURLY_BRACES ~
        top_statement_list ~ T_CLOSE_CURLY_BRACES ^^ { (nsid: NSIdentifier, stmts: List[Statement]) => List(Namespaced(nsid, stmts)) }
      | T_NAMESPACE ~ T_OPEN_CURLY_BRACES ~
        top_statement_list ~ T_CLOSE_CURLY_BRACES ^^ { (stmts: List[Statement]) => List(Namespaced(NSIdentifier(NSResolved, Nil), stmts))  }
      | T_USE ~ use_declarations ~ T_SEMICOLON ^^ id[List[Statement]] _
      | constant_declaration ~ T_SEMICOLON ^^ id[List[ConstantDecl]] _
    ),

    use_declarations -> (
        use_declarations ~ T_COMMA ~ use_declaration ^^ { (uds: List[Statement], ud: Statement) => uds ::: ud :: Nil }
        | use_declaration ^^ inList[Statement] _
    ),

    use_declaration -> (
          namespace_name ^^ {
            (ns: NSIdentifier) =>
              val nns = ns.asResolved
              Import(nns, nns.parts.last)
          }
        | namespace_name ~ T_AS ~ T_STRING ^^ {
            (ns: NSIdentifier, str: String) =>
              val nns = ns.asResolved
              Import(nns, str)
          }
        | T_NS_SEPARATOR ~ namespace_name ^^ {
            (ns: NSIdentifier) =>
              val nns = ns.asResolved
              Import(nns, nns.parts.last)
          }
        | T_NS_SEPARATOR ~ namespace_name ~ T_AS ~ T_STRING ^^ {
            (ns: NSIdentifier, str: String) =>
              val nns = ns.asResolved
              Import(nns, str)
          }
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
          statement ^^ id[Statement] _
        | function_declaration_statement ^^ id[FunctionDecl] _
        | class_declaration_statement ^^ id[Statement] _
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
            (c: Expression, then: List[Statement], elsifs: List[(Expression, Statement)], elze: Option[Statement]) =>
              ((c, Block(then)) :: elsifs).foldLeft(elze) {
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

          (st: List[Statement], cl: StaticClassRef, v: SimpleVariable, clbody: List[Statement], catches: List[Catch]) =>
            Try(Block(st), Catch(cl, v, Block(clbody)) :: catches)
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
          non_empty_additional_catches ^^ id[List[Catch]] _
        | _empty_
    ),

    non_empty_additional_catches -> (
          additional_catch ^^ inList[Catch] _
        | non_empty_additional_catches ~ additional_catch ^^ {
          (ls: List[Catch], c: Catch) =>
            ls ::: List(c)
        }
    ),

    additional_catch -> (
        T_CATCH ~ T_OPEN_BRACES ~ fully_qualified_class_name ~ T_VARIABLE ~ T_CLOSE_BRACES ~ T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ {
          (cl: StaticClassRef, v: SimpleVariable, b: List[Statement]) =>
            Catch(cl, v, Block(b))
        }
    ),

    variable_list -> (
        variable ^^ { (v: Expression) => List(writeable(v)) }
        | variable_list ~ T_COMMA ~ variable ^^ {
          (ls: List[Variable], v: Expression) =>
            ls ::: List(writeable(v))
        }
    ),

    is_reference -> (
          rhs() ^^ { () => None }
        | T_BITWISE_AND ^^ { () => Some(true) }
    ),

    function_declaration_statement -> (
          T_FUNCTION ~ is_reference ~ T_STRING ~
                T_OPEN_BRACES ~ parameter_list ~ T_CLOSE_BRACES ~ T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ {

            (byRef: Option[Boolean], name: String, params: List[ArgumentDecl], body: List[Statement]) =>
              FunctionDecl(idOf(name), params, byRef.isDefined, Block(body))
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
                  (name: String, ifaces: List[StaticClassRef], stmts: (List[MethodDecl], List[PropertyDecl], List[PropertyDecl], List[ClassConstantDecl])) =>
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
        | T_EXTENDS ~ interface_list ^^ id[List[StaticClassRef]] _
    ),

    implements_list -> (
          _empty_
        | T_IMPLEMENTS ~ interface_list ^^ id[List[StaticClassRef]] _
    ),

    interface_list -> (
          fully_qualified_class_name ^^ inList[StaticClassRef] _
        | interface_list ~ T_COMMA ~ fully_qualified_class_name ^^ {
          (ls: List[StaticClassRef], r: StaticClassRef) =>
            ls ::: List(r)
        }
    ),

    foreach_optional_arg -> (
          _emptyOpt_
        | T_DOUBLE_ARROW ~ foreach_variable ^^ inOpt[(Variable, Boolean)] _
    ),

    foreach_variable -> (
          variable ^^ { (v: Expression) => (writeable(v), false) }
        | T_BITWISE_AND ~ variable ^^ { (v: Expression) => (writeable(v), true) }
    ),

    for_statement -> (
          statement ^^ id[Statement] _
        | T_COLON ~ inner_statement_list ~ T_ENDFOR ~ T_SEMICOLON ^^ { (ls: List[Statement]) => Block(ls) }
    ),

    foreach_statement -> (
          statement ^^ id[Statement] _
        | T_COLON ~ inner_statement_list ~ T_ENDFOREACH ~ T_SEMICOLON ^^ { (ls: List[Statement]) => Block(ls) }

    ),

    declare_statement -> (
          statement ^^ id[Statement] _
        | T_COLON ~ inner_statement_list ~ T_ENDDECLARE ~ T_SEMICOLON ^^ { (ls: List[Statement]) => Block(ls) }
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
          T_OPEN_CURLY_BRACES ~ case_list ~ T_CLOSE_CURLY_BRACES ^^ id[List[(Option[Expression], Statement)]] _
        | T_OPEN_CURLY_BRACES ~ T_SEMICOLON ~ case_list ~ T_CLOSE_CURLY_BRACES ^^ id[List[(Option[Expression], Statement)]] _
        | T_COLON ~ case_list ~ T_ENDSWITCH ~ T_SEMICOLON ^^ id[List[(Option[Expression], Statement)]] _
        | T_COLON ~ T_SEMICOLON ~ case_list ~ T_ENDSWITCH ~ T_SEMICOLON ^^ id[List[(Option[Expression], Statement)]] _
    ),

    case_list -> (
          _empty_
        | case_list ~ T_CASE ~ expr ~ case_separator ~ inner_statement_list ^^ {
          (ls: List[(Option[Expression], Statement)], ex: Expression, o: Option[_], st: List[Statement]) =>
            ls ::: List((Some(ex), Block(st)))
        }
        | case_list ~ T_DEFAULT ~ case_separator ~ inner_statement_list ^^ {
          (ls: List[(Option[Expression], Statement)], o: Option[_], st: List[Statement]) =>
            ls ::: List((None, Block(st)))
        }
    ),

    case_separator -> (
          T_COLON ^^ { () => None }
        | T_SEMICOLON ^^ { () => None }
    ),

    while_statement -> (
          statement ^^ id[Statement] _
        | T_COLON ~ inner_statement_list ~ T_ENDWHILE ~ T_SEMICOLON ^^ {
            (ls: List[Statement]) => Block(ls)
        }
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
          (ls: List[(Expression, Statement)], ex: Expression, st: List[Statement]) =>
            ls ::: List((ex, Block(st)))
        }
    ),

    else_single -> (
          _emptyOpt_ /* empty */
        | T_ELSE ~ statement ^^ inOpt[Statement] _
    ),

    new_else_single -> (
          _emptyOpt_ /* empty */
        | T_ELSE ~ T_COLON ~ inner_statement_list ^^ { (ls: List[Statement] ) => Some(Block(ls)) }
    ),

    parameter_list -> (
          non_empty_parameter_list ^^ id[List[ArgumentDecl]] _
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
          non_empty_function_call_parameter_list ^^ id[List[CallArg]] _
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
        | global_var ^^ inList[Variable] _
    ),

    global_var -> (
          T_VARIABLE ^^ { (ex: Expression) => writeable(ex) }
        | T_DOLLAR ~ variable ^^ {
            (ex: Expression) => VariableVariable(ex)
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
            (flags: List[MemberFlag], props: List[PropertyDecl]) =>
              if (flags contains MFStatic) {
                (Nil, props, Nil, Nil)
              } else {
                (Nil, Nil, props, Nil)
              }
          }
        | class_constant_declaration ~ T_SEMICOLON ^^ {
            (ccds: List[ClassConstantDecl]) =>
              (Nil, Nil, Nil, ccds)
          }
        | method_modifiers ~ T_FUNCTION ~ is_reference ~ T_STRING ~ T_OPEN_BRACES ~
                parameter_list ~ T_CLOSE_BRACES ~ method_body ^^ {
            (flags: List[MemberFlag], isRef: Option[Boolean], name: String, params: List[ArgumentDecl], body: Option[Statement]) =>
              (List(MethodDecl(simpleIdOf(name), flags, params, isRef.isDefined, body)), Nil, Nil, Nil)
          }
    ),

    method_body -> (
          T_SEMICOLON ^^ { () => None } /* abstract method */
        | T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ {
            (ls: List[Statement]) => Some(Block(ls))
          }
    ),

    variable_modifiers -> (
          non_empty_member_modifiers ^^ id[List[MemberFlag]] _
        | T_VAR ^^ { () => List(MFPublic) }
    ),

    method_modifiers -> (
          _empty_ /* empty */
        | non_empty_member_modifiers ^^ id[List[MemberFlag]] _
    ),

    non_empty_member_modifiers -> (
          member_modifier ^^ inList[MemberFlag] _
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
          class_variable_declaration ~ T_COMMA ~ T_VARIABLE ^^ {
            (cvs: List[PropertyDecl], v: SimpleVariable) =>
              cvs ::: List(PropertyDecl(v.name, Nil, None))
          }
        | class_variable_declaration ~ T_COMMA ~ T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (cvs: List[PropertyDecl], v: SimpleVariable, e: Expression) =>
              cvs ::: List(PropertyDecl(v.name, Nil, Some(e)))
          }
        | T_VARIABLE ^^ {
            (v: SimpleVariable) =>
              List(PropertyDecl(v.name, Nil, None))
          }
        | T_VARIABLE ~ T_ASSIGN ~ static_expr ^^ {
            (v: SimpleVariable, e: Expression) =>
              List(PropertyDecl(v.name, Nil, Some(e)))
          }
    ),

    class_constant_declaration -> (
          class_constant_declaration ~ T_COMMA ~ T_STRING ~ T_ASSIGN ~ static_expr ^^ {
            (ccs: List[ClassConstantDecl], id: String, e: Expression) =>
            ccs ::: List(ClassConstantDecl(simpleIdOf(id), e))
          }
        | T_CONST ~ T_STRING ~ T_ASSIGN ~ static_expr ^^ {
            (id: String, e: Expression) =>
              List(ClassConstantDecl(simpleIdOf(id), e))
          }
    ),

    echo_expr_list -> (
          echo_expr_list ~ T_COMMA ~ expr ^^ {
            (es: List[Expression], e: Expression) =>
              es ::: List(e)
          }
        | expr ^^ {
            (e: Expression) =>
              List(e)
          }
    ),

    for_expr -> (
          _empty_ /* empty */
        | non_empty_for_expr ^^ id[List[Expression]] _
    ),

    non_empty_for_expr -> (
          non_empty_for_expr ~ T_COMMA ~ expr ^^ {
            (es: List[Expression], e: Expression) =>
              es ::: List(e)
          }
        | expr ^^ {
            (e: Expression) =>
              List(e)
          }

    ),

    lexical_vars -> (
          _empty_ /* empty */
        | T_USE ~ T_OPEN_BRACES ~ lexical_var_list ~ T_CLOSE_BRACES ^^ id[List[ArgumentDecl]] _
    ),

    lexical_var_list -> (
          lexical_var_list ~ T_COMMA ~ T_VARIABLE ^^ {
            (ls: List[ArgumentDecl], v: SimpleVariable) =>
              ls ::: List(ArgumentDecl(v, None, None, false))
          }
        | lexical_var_list ~ T_COMMA ~ T_BITWISE_AND ~ T_VARIABLE ^^ {
            (ls: List[ArgumentDecl], v: SimpleVariable) =>
              ls ::: List(ArgumentDecl(v, None, None, true))
          }
        | T_VARIABLE ^^ {
            (v: SimpleVariable) =>
              List(ArgumentDecl(v, None, None, false))
          }
        | T_BITWISE_AND ~ T_VARIABLE ^^ {
            (v: SimpleVariable) =>
              List(ArgumentDecl(v, None, None, true))
          }

    ),

    function_call -> (
          namespace_name ~ T_OPEN_BRACES ~
                    function_call_parameter_list ~
                    T_CLOSE_BRACES ^^ {
            (nsid: NSIdentifier, args: List[CallArg]) =>
              FunctionCall(StaticFunctionRef(nsid), args)
          }
        | T_NAMESPACE ~ T_NS_SEPARATOR ~ namespace_name ~ T_OPEN_BRACES ~
                    function_call_parameter_list ~
                    T_CLOSE_BRACES ^^ {
            (nsid: NSIdentifier, args: List[CallArg]) =>
              FunctionCall(StaticFunctionRef(nsid.asCurrent), args)
          }
        | T_NS_SEPARATOR ~ namespace_name ~ T_OPEN_BRACES ~
                    function_call_parameter_list ~
                    T_CLOSE_BRACES ^^ {
            (nsid: NSIdentifier, args: List[CallArg]) =>
              FunctionCall(StaticFunctionRef(nsid.asGlobal), args)
          }
        | class_name ~ T_DOUBLE_COLON ~ T_STRING ~ T_OPEN_BRACES ~
                function_call_parameter_list ~
                T_CLOSE_BRACES ^^ {
            (cr: ClassRef, name: String, args: List[CallArg]) =>
              StaticMethodCall(cr, StaticMethodRef(simpleIdOf(name)), args)
          }
        | class_name ~ T_DOUBLE_COLON ~ variable_without_objects ~ T_OPEN_BRACES ~
                function_call_parameter_list ~
                T_CLOSE_BRACES ^^ {
            (cr: ClassRef, name: Variable, args: List[CallArg]) =>
              StaticMethodCall(cr, DynamicMethodRef(name), args)
          }
        | reference_variable ~ T_DOUBLE_COLON ~ T_STRING ~ T_OPEN_BRACES ~
                function_call_parameter_list ~
                T_CLOSE_BRACES ^^ {
            (v: Variable, name: String, args: List[CallArg]) =>
              StaticMethodCall(DynamicClassRef(v), StaticMethodRef(simpleIdOf(name)), args)
          }
        | reference_variable ~ T_DOUBLE_COLON ~ variable_without_objects ~ T_OPEN_BRACES ~
                function_call_parameter_list ~
                T_CLOSE_BRACES ^^ {
            (v: Variable, name: Variable, args: List[CallArg]) =>
              StaticMethodCall(DynamicClassRef(v), DynamicMethodRef(name), args)
          }
        | variable_without_objects ~ T_OPEN_BRACES ~
                function_call_parameter_list ~ T_CLOSE_BRACES ^^ {
            (v: Variable, args: List[CallArg]) =>
              FunctionCall(VarFunctionRef(v), args)
          }
    ),

    class_name -> (
          T_STATIC ^^ {
            () =>
              CalledClass()
          }
        | namespace_name ^^ {
            (nsid: NSIdentifier) =>
              StaticClassRef(nsid)
          }
        | T_NAMESPACE ~ T_NS_SEPARATOR ~ namespace_name ^^ {
            (nsid: NSIdentifier) =>
              StaticClassRef(nsid.asCurrent)
          }
        | T_NS_SEPARATOR ~ namespace_name ^^ {
            (nsid: NSIdentifier) =>
              StaticClassRef(nsid.asGlobal)
          }
    ),

    fully_qualified_class_name -> (
          namespace_name ^^ {
            (nsid: NSIdentifier) =>
              StaticClassRef(nsid)
          }
        | T_NAMESPACE ~ T_NS_SEPARATOR ~ namespace_name ^^ {
            (nsid: NSIdentifier) =>
              StaticClassRef(nsid.asCurrent)
          }
        | T_NS_SEPARATOR ~ namespace_name ^^ {
            (nsid: NSIdentifier) =>
              StaticClassRef(nsid.asGlobal)
          }
    ),

    class_name_reference -> (
          class_name ^^ id[ClassRef] _
        | dynamic_class_name_reference ^^ id[ClassRef] _
    ),

    dynamic_class_name_reference -> (
          base_variable ~ T_OBJECT_OPERATOR ~
                object_property ~ dynamic_class_name_variable_properties ^^ {
            (v: Variable, oa: ObjectAccess, oas: List[ObjectAccess]) =>
              DynamicClassRef(deriveOAList(v, oa :: oas))
          }
        | base_variable ^^ {
            (v: Variable) =>
              VarClassRef(v)
          }
    ),

    dynamic_class_name_variable_properties -> (
          dynamic_class_name_variable_properties ~ T_OBJECT_OPERATOR ~ object_property ^^ {
            (ls: List[ObjectAccess], oa: ObjectAccess) =>
              ls ::: List(oa)
          }
        | _empty_ /* empty */
    ),

    exit_expr -> (
          _emptyOpt_ /* empty */
        | T_OPEN_BRACES ~ T_CLOSE_BRACES ^^ { () => None }
        | T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES  ^^ inOpt[Expression] _
    ),

    backticks_expr -> (
          _emptyOpt_ /* empty */
        | T_STRING ^^ { (s: String) => PHPString(s) }
        | T_ENCAPSED_AND_WHITESPACE  ^^ id[PHPString] _
        | encaps_list ^^ id[Expression] _
    ),

    ctor_arguments -> (
          _empty_ /* empty */
        | T_OPEN_BRACES ~ function_call_parameter_list ~ T_CLOSE_BRACES ^^ id[List[CallArg]] _
    ),

    common_scalar -> (
          T_LNUMBER ^^ id[PHPInteger] _
        | T_DNUMBER ^^ id[PHPFloat] _
        | T_CONSTANT_ENCAPSED_STRING ^^ id[PHPString] _
        | T_LINE        ^^ { () => MCLine() }
        | T_FILE        ^^ { () => MCFile() }
        | T_DIR         ^^ { () => MCDir() }
        | T_CLASS_C     ^^ { () => MCClass() }
        | T_METHOD_C    ^^ { () => MCMethod() }
        | T_FUNC_C      ^^ { () => MCFunction() }
        | T_NS_C        ^^ { () => MCNamespace() }
        | T_START_HEREDOC ~ T_ENCAPSED_AND_WHITESPACE ~ T_END_HEREDOC ^^ id[PHPString] _
        | T_START_HEREDOC ~ T_CONSTANT_ENCAPSED_STRING ~ T_END_HEREDOC ^^ id[PHPString] _
        | T_START_HEREDOC ~ T_END_HEREDOC ^^ { () => PHPString("") }
    ),

    static_expr -> (
          common_scalar ^^ id[Expression] _
        | namespace_name ^^ {
          (nsid: NSIdentifier) =>
            staticConstant(nsid)
          }
        | T_NAMESPACE ~ T_NS_SEPARATOR ~ namespace_name ^^ {
          (nsid: NSIdentifier) =>
            staticConstant(nsid.asCurrent)
          }
        | T_NS_SEPARATOR ~ namespace_name ^^ { (nsid: NSIdentifier) => staticConstant(nsid.asGlobal) }
        | T_PLUS ~ static_expr ^^ id[Expression] _
        | T_MINUS ~ static_expr ^^ {
            (ex: Expression) =>
              Minus(PHPInteger(0), ex)
          }
        | T_ARRAY ~ T_OPEN_BRACES ~ static_array_pair_list ~ T_CLOSE_BRACES ^^ {
            (arrentries: List[(Option[Expression], Expression, Boolean)]) =>
              Array(arrentries)
          }
        | static_class_constant ^^ id[ClassConstant] _
    ),

    static_class_constant -> (
          class_name ~ T_DOUBLE_COLON ~ T_STRING ^^ {
            (cr: ClassRef, str: String) =>
              ClassConstant(cr, simpleIdOf(str))
          }
    ),

    scalar -> (
          class_constant ^^ id[ClassConstant] _
        | namespace_name ^^ {
            (nsid: NSIdentifier) =>
              staticConstant(nsid)
          }
        | T_NAMESPACE ~ T_NS_SEPARATOR ~ namespace_name ^^ {
            (nsid: NSIdentifier) =>
              staticConstant(nsid.asCurrent)
          }
        | T_NS_SEPARATOR ~ namespace_name ^^ {
            (nsid: NSIdentifier) =>
              staticConstant(nsid.asGlobal)
          }
        | common_scalar ^^ id[Expression] _
        | T_DOUBLE_QUOTE ~ encaps_list ~ T_DOUBLE_QUOTE ^^ id[Expression] _
        | T_DOUBLE_QUOTE ~ T_ENCAPSED_AND_WHITESPACE ~ T_DOUBLE_QUOTE ^^ id[PHPString] _
        | T_START_HEREDOC ~ encaps_list ~ T_END_HEREDOC ^^ id[Expression] _
    ),

    static_array_pair_list -> (
          _empty_ /* empty */
        | non_empty_static_array_pair_list ~ possible_comma ^^ {
          (ls: List[(Option[Expression], Expression, Boolean)], s: Option[Boolean]) => ls
        }
    ),

    possible_comma -> (
          rhs() ^^ { () => None } /* empty */
        | T_COMMA ^^ { () => Some(true) }
    ),

    non_empty_static_array_pair_list -> (
          non_empty_static_array_pair_list ~ T_COMMA ~ static_expr ~ T_DOUBLE_ARROW ~ static_expr ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], k: Expression, v: Expression) =>
              ls ::: List((Some(k), v, false))
          }
        | non_empty_static_array_pair_list ~ T_COMMA ~ static_expr ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], v: Expression) =>
              ls ::: List((None, v, false))
          }
        | static_expr ~ T_DOUBLE_ARROW ~ static_expr ^^ {
            (k: Expression, v: Expression) =>
              List((Some(k), v, false))
          }
        | static_expr ^^ {
            (v: Expression) =>
              List((None, v, false))
          }
    ),

    expr -> (
          variable ^^ id[Expression] _
        | T_LIST ~ T_OPEN_BRACES ~  assignment_list ~ T_CLOSE_BRACES ~ T_ASSIGN ~ expr ^^ {
            (vs: List[Option[Variable]], ex: Expression) =>
              ExpandArray(vs, ex)
          }
        | variable ~ T_ASSIGN ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), ex, false)
          }
        | variable ~ T_ASSIGN ~ T_BITWISE_AND ~ variable ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), writeable(ex), true)
          }
        | variable ~ T_ASSIGN ~ T_BITWISE_AND ~ T_NEW ~ class_name_reference ~  ctor_arguments  ^^ {
            (v: Expression, cr: ClassRef, args: List[CallArg]) =>
              Assign(writeable(v), New(cr, args), true)
          }
        | T_NEW ~ class_name_reference ~ ctor_arguments ^^ {
            (cr: ClassRef, args: List[CallArg]) =>
              New(cr, args)
          }
        | T_CLONE ~ expr ^^ {
            (ex: Expression) =>
              Clone(ex)
          }
        | variable ~ T_PLUS_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), Plus(v, ex), false)
          }
        | variable ~ T_MINUS_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), Minus(v, ex), false)
          }
        | variable ~ T_MUL_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), Mult(v, ex), false)
          }
        | variable ~ T_DIV_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), Div(v, ex), false)
          }
        | variable ~ T_CONCAT_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), Concat(v, ex), false)
          }
        | variable ~ T_MOD_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), Mod(v, ex), false)
          }
        | variable ~ T_AND_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), BitwiseAnd(v, ex), false)
          }
        | variable ~ T_OR_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), BitwiseOr(v, ex), false)
          }
        | variable ~ T_XOR_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), BitwiseXor(v, ex), false)
          }
        | variable ~ T_SL_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), ShiftLeft(v, ex), false)
          }
        | variable ~ T_SR_EQUAL ~ expr ^^ {
            (v: Expression, ex: Expression) =>
              Assign(writeable(v), ShiftRight(v, ex), false)
          }
        | variable ~ T_INC ^^ {
            (v: Expression) => PostInc(writeable(v))
          }
        | T_INC ~ variable ^^ {
            (v: Expression) => PreInc(writeable(v))
          }
        | variable ~ T_DEC ^^ {
            (v: Expression) => PostDec(writeable(v))
          }
        | T_DEC ~ variable ^^ {
            (v: Expression) => PreDec(writeable(v))
          }
        | expr ~ T_BOOLEAN_OR ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanOr(ex1, ex2)
          }
        | expr ~ T_BOOLEAN_AND ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanAnd(ex1, ex2)
          }
        | expr ~ T_LOGICAL_OR ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanOr(ex1, ex2)
          }
        | expr ~ T_LOGICAL_AND ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanAnd(ex1, ex2)
          }
        | expr ~ T_LOGICAL_XOR ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanXor(ex1, ex2)
          }
        | expr ~ T_BITWISE_OR ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
               BitwiseOr(ex1, ex2)
          }
        | expr ~ T_BITWISE_AND ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BitwiseAnd(ex1, ex2)
          }
        | expr ~ T_BITWISE_XOR ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BitwiseXor(ex1, ex2)
          }
        | expr ~ T_POINT ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Concat(ex1, ex2)
          }
        | expr ~ T_PLUS ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Plus(ex1, ex2)
          }
        | expr ~ T_MINUS ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Minus(ex1, ex2)
          }
        | expr ~ T_MULT ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Mult(ex1, ex2)
          }
        | expr ~ T_DIV ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Div(ex1, ex2)
          }
        | expr ~ T_MODULO ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Mod(ex1, ex2)
          }
        | expr ~ T_SL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              ShiftLeft(ex1, ex2)
          }
        | expr ~ T_SR ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              ShiftRight(ex1, ex2)
          }
        | T_PLUS ~ expr ^^ {
            (ex: Expression) => Plus(PHPInteger(0), ex)
          }
        | T_MINUS ~ expr ^^ {
            (ex: Expression) => Minus(PHPInteger(0), ex)
          }
        | T_NOT ~ expr ^^ {
            (ex: Expression) => BooleanNot(ex)
          }
        | T_BITWISE_NOT ~ expr ^^ {
            (ex: Expression) => BitwiseNot(ex)
          }
        | expr ~ T_IS_IDENTICAL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Identical(ex1, ex2)
          }
        | expr ~ T_IS_NOT_IDENTICAL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanNot(Identical(ex1, ex2))
          }
        | expr ~ T_IS_EQUAL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              Equal(ex1, ex2)
          }
        | expr ~ T_IS_NOT_EQUAL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanNot(Equal(ex1, ex2))
          }
        | expr ~ T_IS_SMALLER ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
               Smaller(ex1, ex2)
          }
        | expr ~ T_IS_SMALLER_OR_EQUAL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              SmallerEqual(ex1, ex2)
          }
        | expr ~ T_IS_GREATER ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanNot(SmallerEqual(ex1, ex2))
          }
        | expr ~ T_IS_GREATER_OR_EQUAL ~ expr ^^ {
            (ex1: Expression, ex2: Expression) =>
              BooleanNot(Smaller(ex1, ex2))
          }
        | expr ~ T_INSTANCEOF ~ class_name_reference ^^ {
            (ex: Expression, cr: ClassRef) =>
              InstanceOf(ex, cr)
          }
        | T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES ^^ id[Expression] _
        | expr ~ T_QUESTION ~
            expr ~ T_COLON ~
            expr ^^ {
            (c: Expression, t: Expression, e: Expression) =>
              Ternary(c, Some(t), e)
          }
        | expr ~ T_QUESTION ~ T_COLON ~
            expr ^^ {
            (c: Expression, e: Expression) =>
              Ternary(c, None, e)
          }
        | internal_functions_in_yacc ^^ id[Expression] _
        | T_INT_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastInt, ex)
          }
        | T_DOUBLE_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastDouble, ex)
          }
        | T_STRING_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastString, ex)
          }
        | T_ARRAY_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastArray, ex)
          }
        | T_OBJECT_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastObject, ex)
          }
        | T_BOOL_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastBool, ex)
          }
        | T_UNSET_CAST ~ expr ^^ {
            (ex: Expression) =>
              Cast(CastUnset, ex)
          }
        | T_EXIT ~ exit_expr ^^ {
            (ex: Option[Expression]) =>
              Exit(ex)
          }
        | T_AT ~ expr ^^ {
            (ex: Expression) =>
              Silence(ex)
          }
        | scalar ^^ id[Expression] _
        | T_ARRAY ~ T_OPEN_BRACES ~ array_pair_list ~ T_CLOSE_BRACES ^^ {
            (al: List[(Option[Expression], Expression, Boolean)]) =>
              Array(al)
          }
        | T_BACKTICK ~ backticks_expr ~ T_BACKTICK ^^ {
            (cmd: Option[Expression]) =>
              Execute("???")
          }
        | T_PRINT ~ expr ^^ {
            (ex: Expression) =>
              Print(ex)
          }
        | T_FUNCTION ~ is_reference ~ T_OPEN_BRACES ~ 
                parameter_list ~ T_CLOSE_BRACES ~ lexical_vars ~ T_OPEN_CURLY_BRACES ~ inner_statement_list ~ T_CLOSE_CURLY_BRACES ^^ {
            (isRef: Option[Boolean], params: List[ArgumentDecl], vars: List[ArgumentDecl], body: List[Statement]) =>
              Closure(params, vars, isRef.isDefined, Block(body))
          }
    ),

    variable -> (
          base_variable_with_function_calls ~ T_OBJECT_OPERATOR ~
                object_property ~  method_or_not ~ variable_properties ^^ {
            (ex: Expression, oa: ObjectAccess, method: Option[List[CallArg]], ls: List[ObjectAccess]) =>
              deriveOAList(ex, maybeMethod(oa, method) :: ls)
          }
        | base_variable_with_function_calls ^^ id[Expression] _
    ),

    variable_properties -> (
          variable_properties ~ variable_property ^^ {
            (ls: List[ObjectAccess], oa: ObjectAccess) =>
              ls ::: List(oa)
          }
        | _empty_ /* empty */
    ),

    variable_property -> (
          T_OBJECT_OPERATOR ~ object_property ~ method_or_not ^^ {
            (oa: ObjectAccess, method: Option[List[CallArg]]) =>
              maybeMethod(oa, method)
          }
    ),

    method_or_not -> (
          T_OPEN_BRACES ~ function_call_parameter_list ~ T_CLOSE_BRACES ^^ {
            (args: List[CallArg]) => Some(args)
          }
        | _emptyOpt_ /* empty */
    ),

    variable_without_objects -> (
          reference_variable ^^ id[Variable] _
        | simple_indirect_reference ~ reference_variable  ^^ {
            (cnt: IntWrapper, v: Variable) =>
              var r = v
              for (n <- 0 until cnt.v) r = VariableVariable(r)
              r
          }
    ),

    static_member -> (
          class_name ~ T_DOUBLE_COLON ~ variable_without_objects ^^ {
            (cr: ClassRef, v: Variable) =>
              ClassProperty(cr, v)
          }
        | reference_variable ~ T_DOUBLE_COLON ~ variable_without_objects ^^ {
            (cr: Variable, v: Variable) =>
              ClassProperty(VarClassRef(cr), v)
          }
    ),

    base_variable_with_function_calls -> (
          base_variable ^^ id[Variable] _
        | function_call ^^ id[Expression] _
    ),

    base_variable -> (
          reference_variable ^^ id[Variable] _
        | simple_indirect_reference ~ reference_variable ^^ {
            (cnt: IntWrapper, v: Variable) =>
              var r = v
              for (n <- 0 until cnt.v) r = VariableVariable(r)
              r
          }
        | static_member ^^ id[ClassProperty] _
    ),

    reference_variable -> (
          reference_variable ~ T_OPEN_RECT_BRACES ~ dim_offset ~ T_CLOSE_RECT_BRACES ^^ {
            (v: Variable, off: Option[Expression]) =>
              off match {
                case Some(ex) => ArrayEntry(v, ex)
                case None => NextArrayEntry(v)
              }
          }
        | reference_variable ~ T_OPEN_CURLY_BRACES ~ expr ~ T_CLOSE_CURLY_BRACES ^^ {
            (v: Variable, off: Expression) =>
              ArrayEntry(v, off)
          }
        | compound_variable ^^ id[Variable] _
    ),

    compound_variable -> (
          T_VARIABLE ^^ id[SimpleVariable] _
        | T_DOLLAR ~ T_OPEN_CURLY_BRACES ~ expr ~ T_CLOSE_CURLY_BRACES ^^ {
            (ex: Expression) =>
              VariableVariable(ex)
          }
    ),

    dim_offset -> (
          _emptyOpt_ /* empty */
        | expr ^^ inOpt[Expression] _
    ),

    object_property -> (
          object_dim_list ^^ id[ObjectAccess] _
        | variable_without_objects ^^ { (ex: Expression) => OAExpression(ex) }
    ),

    object_dim_list -> (
          object_dim_list ~ T_OPEN_RECT_BRACES ~ dim_offset ~ T_CLOSE_RECT_BRACES ^^ {
            (oa: ObjectAccess, off: Option[Expression]) =>
              oa match {
                case OAArray(b, offs) =>
                  OAArray(b, offs ::: List(off))
                case oa: OAScalar =>
                  OAArray(oa, List(off))
              }
          }
        | object_dim_list ~ T_OPEN_CURLY_BRACES ~ expr ~ T_CLOSE_CURLY_BRACES ^^ {
            (oa: ObjectAccess, ex: Expression) =>
              oa match {
                case OAArray(b, offs) =>
                  OAArray(b, offs ::: List(Some(ex)))
                case oa: OAScalar =>
                  OAArray(oa, List(Some(ex)))
              }
          }
        | variable_name ^^ id[ObjectAccess] _
    ),

    variable_name -> (
          T_STRING ^^ {
            (str: String) =>
              OAIdentifier(simpleIdOf(str))
          }
        | T_OPEN_CURLY_BRACES ~ expr ~ T_CLOSE_CURLY_BRACES ^^ {
            (ex: Expression) =>
              OAExpression(ex)
          }
    ),

    simple_indirect_reference -> (
          T_DOLLAR ^^ {
            () => new IntWrapper(1)
          }
        | simple_indirect_reference ~ T_DOLLAR ^^ {
            (w: IntWrapper) => w.copy(v = w.v+1)
          }
    ),

    assignment_list -> (
          assignment_list ~ T_COMMA ~ assignment_list_element ^^ {
            (ls: List[Option[Variable]], v: Option[Variable]) =>
              ls ::: List(v)
          }
        | assignment_list_element ^^ {
            (v: Option[Variable]) =>
              List(v)
          }
    ),

    assignment_list_element -> (
          variable ^^ { (v: Expression) => Some(v) }
        | T_LIST ~ T_OPEN_BRACES ~  assignment_list ~ T_CLOSE_BRACES ^^ {
            (ls: List[Option[Variable]]) =>
              Some(ListVar(ls))
          }
        | _emptyOpt_ /* empty */
    ),

    array_pair_list -> (
          _empty_ /* empty */
        | non_empty_array_pair_list ~ possible_comma  ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], b: Option[Boolean]) =>
              ls
          }
    ),

    non_empty_array_pair_list -> (
          non_empty_array_pair_list ~ T_COMMA ~ expr ~ T_DOUBLE_ARROW ~ expr ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], k: Expression, v: Expression) =>
              ls ::: List((Some(k), v, false))
          }
        | non_empty_array_pair_list ~ T_COMMA ~ expr ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], v: Expression) =>
              ls ::: List((None, v, false))
          }
        | expr ~ T_DOUBLE_ARROW ~ expr ^^ {
            (k: Expression, v: Expression) =>
              List((Some(k), v, false))
          }
        | expr ^^ {
            (v: Expression) =>
              List((None, v, false))
          }
        | non_empty_array_pair_list ~ T_COMMA ~ expr ~ T_DOUBLE_ARROW ~ T_BITWISE_AND ~ variable ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], k: Expression, v: Expression) =>
              ls ::: List((Some(k), v, true))
          }
        | non_empty_array_pair_list ~ T_COMMA ~ T_BITWISE_AND ~ variable ^^ {
            (ls: List[(Option[Expression], Expression, Boolean)], v: Expression) =>
              ls ::: List((None, v, true))
          }
        | expr ~ T_DOUBLE_ARROW ~ T_BITWISE_AND ~ variable ^^ {
            (k: Expression, v: Expression) =>
              List((Some(k), v, true))
          }
        | T_BITWISE_AND ~ variable ^^ {
            (v: Expression) =>
              List((None, v, true))
          }
    ),

    encaps_list -> (
          encaps_list ~ encaps_var ^^ {
            (e: Expression, v: Variable) =>
              Concat(e, v)
          }
        | encaps_list ~ T_ENCAPSED_AND_WHITESPACE ^^ {
            (e: Expression, v: Expression) =>
              Concat(e, v)
          }
        | encaps_var ^^ {
            (e: Variable) =>
              e
          }
        | T_ENCAPSED_AND_WHITESPACE ~ encaps_var ^^ {
            (e: Expression, v: Variable) =>
              Concat(e, v)
          }
    ),


    encaps_var -> (
          T_VARIABLE  ^^ id[SimpleVariable] _
        | T_VARIABLE ~ T_OPEN_RECT_BRACES ~ encaps_var_offset ~ T_CLOSE_RECT_BRACES ^^ {
            (v: SimpleVariable, off: Expression) =>
              ArrayEntry(v, off)
          }
        | T_VARIABLE ~ T_OBJECT_OPERATOR ~ T_STRING ^^ {
            (v: SimpleVariable, s: String) =>
              ObjectProperty(v, simpleIdOf(s))
          }
        | T_DOLLAR_OPEN_CURLY_BRACES ~ T_STRING_VARNAME ~ T_CLOSE_CURLY_BRACES ^^ {
            (v: String) => SimpleVariable(simpleIdOf(v))
          }
        | T_DOLLAR_OPEN_CURLY_BRACES ~ expr ~ T_CLOSE_CURLY_BRACES ^^ {
            (e: Expression) => VariableVariable(e)
          }
        | T_DOLLAR_OPEN_CURLY_BRACES ~ T_STRING_VARNAME ~ T_OPEN_RECT_BRACES ~ expr ~ T_CLOSE_RECT_BRACES ~ T_CLOSE_CURLY_BRACES ^^ {
            (v: String, off: Expression) =>
              ArrayEntry(SimpleVariable(simpleIdOf(v)), off)
          }
        | T_CURLY_OPEN ~ variable ~ T_CLOSE_CURLY_BRACES ^^ id[Expression] _
    ),

    encaps_var_offset -> (
          T_STRING ^^ {
            (s: String) => PHPString(s)
          }
        | T_NUM_STRING ^^ {
            (s: String) => PHPString(s)
          }
        | T_VARIABLE ^^ id[SimpleVariable] _
    ),

    internal_functions_in_yacc -> (
          T_ISSET ~ T_OPEN_BRACES ~ isset_variables ~ T_CLOSE_BRACES ^^ {
            (vs: List[Variable]) => Isset(vs)
          }
        | T_EMPTY ~ T_OPEN_BRACES ~ variable ~ T_CLOSE_BRACES ^^ {
            (v: Expression) => Empty(writeable(v))
          }
        | T_INCLUDE ~ expr ^^ {
            (e: Expression) => Include(e, false)
          }
        | T_INCLUDE_ONCE ~ expr ^^ {
            (e: Expression) => Include(e, true)
          }
        | T_EVAL ~ T_OPEN_BRACES ~ expr ~ T_CLOSE_BRACES ^^ {
            (e: Expression) => Eval(e)
          }
        | T_REQUIRE ~ expr ^^ {
            (e: Expression) => Require(e, false)
          }
        | T_REQUIRE_ONCE ~ expr ^^ {
            (e: Expression) => Require(e, true)
          }
    ),

    isset_variables -> (
          variable ^^ {
            (ex: Expression) => List(writeable(ex))
          }
        | isset_variables ~ T_COMMA ~ variable ^^ {
          (vs: List[Variable], v: Expression) => vs ::: List(writeable(v))
        }
    ),

    class_constant -> (
          class_name ~ T_DOUBLE_COLON ~ T_STRING ^^ {
            (cr: ClassRef, s: String) =>
              ClassConstant(cr, simpleIdOf(s))
          }
        | reference_variable ~ T_DOUBLE_COLON ~ T_STRING ^^ {
            (v: Variable, s: String) =>
              ClassConstant(VarClassRef(v), simpleIdOf(s))
          }
    )
  )
}
