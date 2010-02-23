/**
 * PHP 5.3 JFlex specification file
 * Based on a php4 version from Nenad Jovanovic */

package phpanalysis.parser;

import java.util.*;
import java_cup.runtime.*;

%%

%{

    private StringBuffer morePrefix;
    private boolean clearMorePrefix;

    public ArrayList<Comment> comments;

    public class Comment {
        public int line;
        public int col;
        public String filename;
        public String content;
        public Comment(int _line, int _col, String _filename, String _content) {
            line = _line;
            col = _col;
            filename = _filename;
            content = _content;
        }
    }

    public void registerComment() {
        Comment c = new Comment(yyline + 1, yycolumn, getFileName(), text());

        comments.add(c);
    }
    // same functionality as Flex's yymore()
    public void cleanMore() {
        this.morePrefix.setLength(0);
        this.clearMorePrefix = true;
    }
    public void more() {
        this.morePrefix.append(this.yytext());
        this.clearMorePrefix = false;
    }

    // wrapper around yytext() allowing the usage of more()
    public final String text() {
        return (this.morePrefix.toString() + this.yytext());
    }

    // wrapper around yylength() allowing the usage of more()
    public final int length() {
        return this.morePrefix.length() + this.yylength();
    }

    // wrapper around yycharat() allowing the usage of more()
    public final char charat(int pos) {
        if (pos < this.morePrefix.length()) {
            return this.morePrefix.charAt(pos);
        } else {
            return this.yycharat(pos - this.morePrefix.length());
        }
    }

    // wrapper around yylex() deleting the morePrefix
    public Symbol lex() throws java.io.IOException {
        Symbol ret = this.next_token();
        this.morePrefix.setLength(0);
        this.clearMorePrefix = true;
        return ret;
    }

    private LinkedList stateStack;

    private String heredocLabel;

    private String fileName;

    // same functionality as Flex's yy_push_state
    private void pushState(int state) {
        this.stateStack.add(new Integer(this.yystate()));
        yybegin(state);
    }

    // same functionality as Flex's yy_pop_state
    private void popState() {
        yybegin(((Integer) this.stateStack.removeLast()).intValue());
    }

    // same functionality as Flex's yy_top_state
    private int topState() {
        return ((Integer) this.stateStack.getLast()).intValue();
    }

    // shorthand for constructing Symbol objects
    private Symbol symbol(int type, String name) {
        return symbol(type, name, text());
    }

    private Symbol symbol(int type, String name, String content) {
        // use the Symbol's "left value" as line number
        int line = yyline + 1;
        return new Symbol(
            type, 
            line, 
            -1, 
            new ParseNode(type, name, content, line, yycolumn, getFileName()));
    }

    // always call this method after constructing the lexer object
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getFileName() {
        if (this.fileName == null) {
            throw new RuntimeException("fileName not set in lexer object");
        }
        return this.fileName;
    }

    public boolean isLabelStart(char c) {
        return ((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z') || (c) == '_' || (c) >= 0x7F;
    }
%}



%init{

    this.stateStack = new LinkedList();
    this.comments   = new ArrayList<Comment>();
    this.morePrefix = new StringBuffer();
    this.clearMorePrefix = true;

%init}


%x ST_IN_SCRIPTING
%x ST_DOUBLE_QUOTES
%x ST_SINGLE_QUOTE
%x ST_BACKQUOTE
%x ST_HEREDOC
%x ST_LOOKING_FOR_PROPERTY
%x ST_LOOKING_FOR_VARNAME
%x ST_VAR_OFFSET
%x ST_COMMENT
%x ST_ONE_LINE_COMMENT

LNUM = [0-9]+
DNUM = ([0-9]*[\.][0-9]+)|([0-9]+[\.][0-9]*)
EXPONENT_DNUM = (({LNUM}|{DNUM})[eE][+\-]?{LNUM})
HNUM = "0x"[0-9a-fA-F]+
// a few special characters are not matched by LABEL in jflex although they
// are matched by flex (e.g. in very few language files of PHPNuke 7.5); encoding problem?
LABEL = [a-zA-Z_\x7f-\xbb\xbc-\xde\xdf-\xff][a-zA-Z0-9_\x7f-\xbb\xbc-\xde\xdf-\xff]*
LABEL_FIRST = [a-zA-Z_\x7f-\xbb\xbc-\xde\xdf-\xff]

WHITESPACE = [ \n\r\t]+
TABS_AND_SPACES = [ \t]*
// we don't need TOKENS and ENCAPSED_TOKENS any longer since we had to split up the
// rules for them (because of CUP, which doesn't support character tokens)
// TOKENS = [;:,.\[\]()|\^&+\-/*=%!~$<>?@]
// ENCAPSED_TOKENS = [\[\]{}$]
ANY_CHAR = (.|[\n])
NEWLINE = ("\r"|"\n"|"\r\n")

// using 8bit (mimicking flex) doesn't work properly, so use unicode
%unicode
%line
%column
%ignorecase
%cupsym Symbols
%cup
%class Lexer
%public

%%

<ST_IN_SCRIPTING> {
    "exit" { return symbol(Symbols.T_EXIT, "T_EXIT"); }
    "die" { return symbol(Symbols.T_EXIT, "T_EXIT"); }
    "function"|"cfunction" { return symbol(Symbols.T_FUNCTION, "T_FUNCTION"); }
    "const" { return symbol(Symbols.T_CONST, "T_CONST"); }
    "return" { return symbol(Symbols.T_RETURN, "T_RETURN"); }
    "if" { return symbol(Symbols.T_IF, "T_IF"); }
    "elseif" { return symbol(Symbols.T_ELSEIF, "T_ELSEIF"); }
    "endif" { return symbol(Symbols.T_ENDIF, "T_ENDIF"); }
    "else" { return symbol(Symbols.T_ELSE, "T_ELSE"); }
    "while" { return symbol(Symbols.T_WHILE, "T_WHILE"); }
    "endwhile" { return symbol(Symbols.T_ENDWHILE, "T_ENDWHILE"); }
    "do" { return symbol(Symbols.T_DO, "T_DO"); }
    "for" { return symbol(Symbols.T_FOR, "T_FOR"); }
    "endfor" { return symbol(Symbols.T_ENDFOR, "T_ENDFOR"); }
    "foreach" { return symbol(Symbols.T_FOREACH, "T_FOREACH"); }
    "endforeach" { return symbol(Symbols.T_ENDFOREACH, "T_ENDFOREACH"); }
    "declare" { return symbol(Symbols.T_DECLARE, "T_DECLARE"); }
    "enddeclare" { return symbol(Symbols.T_ENDDECLARE, "T_ENDDECLARE"); }
    "as" { return symbol(Symbols.T_AS, "T_AS"); }
    "switch" { return symbol(Symbols.T_SWITCH, "T_SWITCH"); }
    "endswitch" { return symbol(Symbols.T_ENDSWITCH, "T_ENDSWITCH"); }
    "case" { return symbol(Symbols.T_CASE, "T_CASE"); }
    "default" { return symbol(Symbols.T_DEFAULT, "T_DEFAULT"); }
    "break" { return symbol(Symbols.T_BREAK, "T_BREAK"); }
    "continue" { return symbol(Symbols.T_CONTINUE, "T_CONTINUE"); }
    "echo" { return symbol(Symbols.T_ECHO, "T_ECHO"); }
    "try" { return symbol(Symbols.T_TRY, "T_TRY"); }
    "catch" { return symbol(Symbols.T_CATCH, "T_CATCH"); }
    "throw" { return symbol(Symbols.T_THROW, "T_THROW"); }
    "interface" { return symbol(Symbols.T_INTERFACE, "T_INTERFACE"); }
    "instanceof" { return symbol(Symbols.T_INSTANCEOF, "T_INSTANCEOF"); }
    "implements" { return symbol(Symbols.T_IMPLEMENTS, "T_IMPLEMENTS"); }
    "clone" { return symbol(Symbols.T_CLONE, "T_CLONE"); }
    "__halt_compiler" { return symbol(Symbols.T_HALT_COMPILER, "T_HALT_COMPILER"); }
    "abstract" { return symbol(Symbols.T_ABSTRACT, "T_ABSTRACT"); }
    "private" { return symbol(Symbols.T_PRIVATE, "T_PRIVATE"); }
    "protected" { return symbol(Symbols.T_PROTECTED, "T_PROTECTED"); }
    "public" { return symbol(Symbols.T_PUBLIC, "T_PUBLIC"); }
    "final" { return symbol(Symbols.T_FINAL, "T_FINAL"); }
    "print" { return symbol(Symbols.T_PRINT, "T_PRINT"); }
    "class" { return symbol(Symbols.T_CLASS, "T_CLASS"); }
    "extends" { return symbol(Symbols.T_EXTENDS, "T_EXTENDS"); }
    "::" { return symbol(Symbols.T_DOUBLE_COLON, "T_DOUBLE_COLON"); }
    "new" { return symbol(Symbols.T_NEW, "T_NEW"); }
    "var" { return symbol(Symbols.T_VAR, "T_VAR"); }
    "eval" { return symbol(Symbols.T_EVAL, "T_EVAL"); }
    "include" { return symbol(Symbols.T_INCLUDE, "T_INCLUDE"); }
    "include_once" { return symbol(Symbols.T_INCLUDE_ONCE, "T_INCLUDE_ONCE"); }
    "require" { return symbol(Symbols.T_REQUIRE, "T_REQUIRE"); }
    "require_once" { return symbol(Symbols.T_REQUIRE_ONCE, "T_REQUIRE_ONCE"); }
    "use" { return symbol(Symbols.T_USE, "T_USE"); }
    "global" { return symbol(Symbols.T_GLOBAL, "T_GLOBAL"); }
    "isset" { return symbol(Symbols.T_ISSET, "T_ISSET"); }
    "empty" { return symbol(Symbols.T_EMPTY, "T_EMPTY"); }
    "static" { return symbol(Symbols.T_STATIC, "T_STATIC"); }
    "unset" { return symbol(Symbols.T_UNSET, "T_UNSET"); }
    "=>" { return symbol(Symbols.T_DOUBLE_ARROW, "T_DOUBLE_ARROW"); }
    "list" { return symbol(Symbols.T_LIST, "T_LIST"); }
    "array" { return symbol(Symbols.T_ARRAY, "T_ARRAY"); }
    "++" { return symbol(Symbols.T_INC, "T_INC"); }
    "--" { return symbol(Symbols.T_DEC, "T_DEC"); }
    "===" { return symbol(Symbols.T_IS_IDENTICAL, "T_IS_IDENTICAL"); }
    "!==" { return symbol(Symbols.T_IS_NOT_IDENTICAL, "T_IS_NOT_IDENTICAL"); }
    "==" { return symbol(Symbols.T_IS_EQUAL, "T_IS_EQUAL"); }
    "!="|"<>" { return symbol(Symbols.T_IS_NOT_EQUAL, "T_IS_NOT_EQUAL"); }
    "<=" { return symbol(Symbols.T_IS_SMALLER_OR_EQUAL, "T_IS_SMALLER_OR_EQUAL"); }
    ">=" { return symbol(Symbols.T_IS_GREATER_OR_EQUAL, "T_IS_GREATER_OR_EQUAL"); }
    "+=" { return symbol(Symbols.T_PLUS_EQUAL, "T_PLUS_EQUAL"); }
    "-=" { return symbol(Symbols.T_MINUS_EQUAL, "T_MINUS_EQUAL"); }
    "*=" { return symbol(Symbols.T_MUL_EQUAL, "T_MUL_EQUAL"); }
    "/=" { return symbol(Symbols.T_DIV_EQUAL, "T_DIV_EQUAL"); }
    ".=" { return symbol(Symbols.T_CONCAT_EQUAL, "T_CONCAT_EQUAL"); }
    "%=" { return symbol(Symbols.T_MOD_EQUAL, "T_MOD_EQUAL"); }
    "<<=" { return symbol(Symbols.T_SL_EQUAL, "T_SL_EQUAL"); }
    ">>=" { return symbol(Symbols.T_SR_EQUAL, "T_SR_EQUAL"); }
    "&=" { return symbol(Symbols.T_AND_EQUAL, "T_AND_EQUAL"); }
    "|=" { return symbol(Symbols.T_OR_EQUAL, "T_OR_EQUAL"); }
    "^=" { return symbol(Symbols.T_XOR_EQUAL, "T_XOR_EQUAL"); }
    "||" { return symbol(Symbols.T_BOOLEAN_OR, "T_BOOLEAN_OR"); }
    "&&" { return symbol(Symbols.T_BOOLEAN_AND, "T_BOOLEAN_AND"); }
    "OR" { return symbol(Symbols.T_LOGICAL_OR, "T_LOGICAL_OR"); }
    "AND" { return symbol(Symbols.T_LOGICAL_AND, "T_LOGICAL_AND"); }
    "XOR" { return symbol(Symbols.T_LOGICAL_XOR, "T_LOGICAL_XOR"); }
    "<<" { return symbol(Symbols.T_SL, "T_SL"); }
    ">>" { return symbol(Symbols.T_SR, "T_SR"); }

// Single char Tokens
    ";" { return symbol(Symbols.T_SEMICOLON, "T_SEMICOLON"); }
    ":" { return symbol(Symbols.T_COLON, "T_COLON"); }
    "," { return symbol(Symbols.T_COMMA, "T_COMMA"); }
    "." { return symbol(Symbols.T_POINT, "T_POINT"); }
    "[" { return symbol(Symbols.T_OPEN_RECT_BRACES, "T_OPEN_RECT_BRACES"); }
    "]" { return symbol(Symbols.T_CLOSE_RECT_BRACES, "T_CLOSE_RECT_BRACES"); }
    "(" { return symbol(Symbols.T_OPEN_BRACES, "T_OPEN_BRACES"); }
    ")" { return symbol(Symbols.T_CLOSE_BRACES, "T_CLOSE_BRACES"); }
    "|" { return symbol(Symbols.T_BITWISE_OR, "T_BITWISE_OR"); }
    "^" { return symbol(Symbols.T_BITWISE_XOR, "T_BITWISE_XOR"); }
    "&" { return symbol(Symbols.T_BITWISE_AND, "T_BITWISE_AND"); }
    "+" { return symbol(Symbols.T_PLUS, "T_PLUS"); }
    "-" { return symbol(Symbols.T_MINUS, "T_MINUS"); }
    "/" { return symbol(Symbols.T_DIV, "T_DIV"); }
    "*" { return symbol(Symbols.T_MULT, "T_MULT"); }
    "=" { return symbol(Symbols.T_ASSIGN, "T_ASSIGN"); }
    "%" { return symbol(Symbols.T_MODULO, "T_MODULO"); }
    "!" { return symbol(Symbols.T_NOT, "T_NOT"); }
    "~" { return symbol(Symbols.T_BITWISE_NOT, "T_BITWISE_NOT"); }
    "$" { return symbol(Symbols.T_DOLLAR, "T_DOLLAR"); }
    "<" { return symbol(Symbols.T_IS_SMALLER, "T_IS_SMALLER"); }
    ">" { return symbol(Symbols.T_IS_GREATER, "T_IS_GREATER"); }
    "?" { return symbol(Symbols.T_QUESTION, "T_QUESTION"); }
    "@" { return symbol(Symbols.T_AT, "T_AT"); }

    "__METHOD__" { return symbol(Symbols.T_METHOD_C, "T_METHOD_C"); }
    "__CLASS__" { return symbol(Symbols.T_CLASS_C, "T_CLASS_C"); }
    "__FUNCTION__" { return symbol(Symbols.T_FUNC_C, "T_FUNC_C"); }
    "__LINE__" { return symbol(Symbols.T_LINE, "T_LINE"); }
    "__FILE__" { return symbol(Symbols.T_FILE, "T_FILE"); }
    {LNUM}|{HNUM} { return symbol(Symbols.T_LNUMBER, "T_LNUMBER"); }
    {DNUM}|{EXPONENT_DNUM} { return symbol(Symbols.T_DNUMBER, "T_DNUMBER"); }
}

<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("int"|"integer"){TABS_AND_SPACES}")" { return symbol(Symbols.T_INT_CAST, "T_INT_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("real"|"double"|"float"){TABS_AND_SPACES}")" { return symbol(Symbols.T_DOUBLE_CAST, "T_DOUBLE_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"binary"{TABS_AND_SPACES}")" { return symbol(Symbols.T_STRING_CAST, "T_STRING_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"string"{TABS_AND_SPACES}")" { return symbol(Symbols.T_STRING_CAST, "T_STRING_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"array"{TABS_AND_SPACES}")" { return symbol(Symbols.T_ARRAY_CAST, "T_ARRAY_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"object"{TABS_AND_SPACES}")" { return symbol(Symbols.T_OBJECT_CAST, "T_OBJECT_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("bool"|"boolean"){TABS_AND_SPACES}")" { return symbol(Symbols.T_BOOL_CAST, "T_BOOL_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("unset"){TABS_AND_SPACES}")" { return symbol(Symbols.T_UNSET_CAST, "T_UNSET_CAST"); }

<ST_DOUBLE_QUOTES>[\"] {
    yybegin(ST_IN_SCRIPTING);
    return symbol(Symbols.T_DOUBLE_QUOTE, "T_DOUBLE_QUOTE");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC>"{$" {
    pushState(ST_IN_SCRIPTING);
    yypushback(1);
    return symbol(Symbols.T_CURLY_OPEN, "T_CURLY_OPEN");
}

<ST_DOUBLE_QUOTES>{ANY_CHAR} {
    scanner:
    while(zzMarkedPos < zzEndRead) {
        switch(zzBuffer[zzMarkedPos-1]) {
            case '"':
                zzMarkedPos--;
                break scanner;
            case '$':
                if (zzBuffer[zzMarkedPos] == '{' || isLabelStart(zzBuffer[zzMarkedPos])) {
                    zzMarkedPos--;
                    break scanner;
                }
                break;
            case '{':
                if (zzBuffer[zzMarkedPos] == '$') {
                    zzMarkedPos--;
                    break scanner;
                }
                break;
            case '\\':
                zzMarkedPos++;
        }
        zzMarkedPos++;
    }

    return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC> "$" {LABEL} "->" {LABEL_FIRST} {
    yypushback(3);
    pushState(ST_LOOKING_FOR_PROPERTY);
    return symbol(Symbols.T_VARIABLE, "T_VARIABLE");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC> "$" {LABEL} "["  {
    yypushback(1);
    pushState(ST_VAR_OFFSET);
    return symbol(Symbols.T_VARIABLE, "T_VARIABLE");
}

<ST_IN_SCRIPTING,ST_DOUBLE_QUOTES,ST_HEREDOC,ST_BACKQUOTE,ST_VAR_OFFSET> "$" {LABEL} {
    return symbol(Symbols.T_VARIABLE, "T_VARIABLE");
}

<ST_VAR_OFFSET>"[" {
    return symbol(Symbols.T_OPEN_RECT_BRACES, "T_OPEN_RECT_BRACES");
}

<ST_VAR_OFFSET>"]" {
    popState();
    return symbol(Symbols.T_CLOSE_RECT_BRACES, "T_CLOSE_RECT_BRACES");
}

<ST_IN_SCRIPTING>"->" {
	pushState(ST_LOOKING_FOR_PROPERTY);
    return symbol(Symbols.T_OBJECT_OPERATOR, "T_OBJECT_OPERATOR");
}

<ST_LOOKING_FOR_PROPERTY>{LABEL} {
	popState();
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_LOOKING_FOR_PROPERTY>{ANY_CHAR} {
	yypushback(length());
	popState();
}

<ST_IN_SCRIPTING>"{" {
	pushState(ST_IN_SCRIPTING);
    return symbol(Symbols.T_OPEN_CURLY_BRACES, "T_OPEN_CURLY_BRACES");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC>"${" {
	pushState(ST_LOOKING_FOR_VARNAME);
    return symbol(Symbols.T_DOLLAR_OPEN_CURLY_BRACES, "T_DOLLAR_OPEN_CURLY_BRACES");
}

<ST_IN_SCRIPTING>"}" {
    // TODO: could make problems
	// if (yy_start_stack_ptr) {
	//	yy_pop_state();
	// }
    
//    System.out.println("POPPING STATE!!!");
    popState();
    return symbol(Symbols.T_CLOSE_CURLY_BRACES, "T_CLOSE_CURLY_BRACES");
}

<ST_LOOKING_FOR_VARNAME>{LABEL} {
	popState();
	pushState(ST_IN_SCRIPTING);
    return symbol(Symbols.T_STRING_VARNAME, "T_STRING_VARNAME");
}

<ST_LOOKING_FOR_VARNAME>{ANY_CHAR} {
	yypushback(length());
	popState();
	pushState(ST_IN_SCRIPTING);
}

<ST_VAR_OFFSET>{LNUM}|{HNUM} {
    return symbol(Symbols.T_NUM_STRING, "T_NUM_STRING");
}

// <YYINITIAL>(([^<]|"<"[^?%s<]){1,400})|"<s"|"<" {
<YYINITIAL>(([^<]|"<"[^?%s<])*)|"<s"|"<" {
    // NJ: replaced {1,400} by * (because it's faster)
    return symbol(Symbols.T_INLINE_HTML, "T_INLINE_HTML");
}

<YYINITIAL>"<?"|"<script"{WHITESPACE}+"language"{WHITESPACE}*"="{WHITESPACE}*("php"|"\"php\""|"\'php\'"){WHITESPACE}*">" {
    yybegin(ST_IN_SCRIPTING);
//    return symbol(Symbols.T_OPEN_TAG, "T_OPEN_TAG");
}

<YYINITIAL>"<%="|"<?=" {
    yybegin(ST_IN_SCRIPTING);
    //return new Yytoken("T_ECHO", text());
//    return symbol(Symbols.T_OPEN_TAG_WITH_ECHO, "T_OPEN_TAG_WITH_ECHO");
}

<YYINITIAL>"<%" {
    yybegin(ST_IN_SCRIPTING);
//    return symbol(Symbols.T_OPEN_TAG, "T_OPEN_TAG");
}

<YYINITIAL>"<?php"([ \t]|{NEWLINE}) {
	yybegin(ST_IN_SCRIPTING);
//    return symbol(Symbols.T_OPEN_TAG, "T_OPEN_TAG");
}


<ST_IN_SCRIPTING,ST_VAR_OFFSET>{LABEL} {
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_IN_SCRIPTING>{WHITESPACE} {
    // don't return this token, since the parser has no rule for it;
    // in the orignal PHP sources, this filtering is not performed inside
    // the lexer, but by a function that is located between the parser and
    // the lexer (this function has the name zendlex())
	// return T_WHITESPACE;
}

<ST_IN_SCRIPTING>"#"|"//" {
	yybegin(ST_ONE_LINE_COMMENT);
    more();
}

<ST_ONE_LINE_COMMENT>"?"|"%"|">" {
	more();
}

<ST_ONE_LINE_COMMENT>[^\n\r?%>]+ {
	more();
}

<ST_ONE_LINE_COMMENT>{NEWLINE} {
    registerComment();
    cleanMore();
	yybegin(ST_IN_SCRIPTING);
}

<ST_ONE_LINE_COMMENT>"?>"|"%>" {
    registerComment();
    cleanMore();
    yypushback(2);
    yybegin(ST_IN_SCRIPTING);
}

<ST_IN_SCRIPTING>"/*" {
	yybegin(ST_COMMENT);
	more();
}

<ST_COMMENT>[^*]+ {
	more();
}

<ST_COMMENT>"*/" {
	yybegin(ST_IN_SCRIPTING);
    registerComment();
    cleanMore();
}

<ST_COMMENT>"*" {
	more();
}

<ST_IN_SCRIPTING>("?>"|"</script"{WHITESPACE}*">"){NEWLINE}? {
	yybegin(YYINITIAL);
    return symbol(Symbols.T_SEMICOLON, "T_SEMICOLON");
}


<ST_IN_SCRIPTING>"%>"{NEWLINE}? {
    yybegin(YYINITIAL);
    return symbol(Symbols.T_SEMICOLON, "T_SEMICOLON");
}

<ST_IN_SCRIPTING>b?([\"]([^$\"\\]|("\\".))*[\"]) {
    return symbol(Symbols.T_CONSTANT_ENCAPSED_STRING, "T_CONSTANT_ENCAPSED_STRING", text().substring(1, text().length()-1));
}

<ST_IN_SCRIPTING>b?([']([^'\\]|("\\".))*[']) {
    return symbol(Symbols.T_CONSTANT_ENCAPSED_STRING, "T_CONSTANT_ENCAPSED_STRING", text().substring(1, text().length()-1));
}

<ST_IN_SCRIPTING>b?[\"] {
    yybegin(ST_DOUBLE_QUOTES);
    return symbol(Symbols.T_DOUBLE_QUOTE, "T_DOUBLE_QUOTE");
}

<ST_IN_SCRIPTING>b?"<<<"{TABS_AND_SPACES}{LABEL}{NEWLINE} {
    // start of heredoc

    // determine heredoc label and save it for later use
    this.heredocLabel = text().substring(3).trim();

    yybegin(ST_HEREDOC);
    return symbol(Symbols.T_START_HEREDOC, "T_START_HEREDOC");
}

<ST_IN_SCRIPTING>[`] {
	yybegin(ST_BACKQUOTE);
    return symbol(Symbols.T_BACKTICK, "T_BACKTICK");
}


<ST_IN_SCRIPTING>b?['] {
	yybegin(ST_SINGLE_QUOTE);
}

<ST_HEREDOC>^{LABEL}(";")?{NEWLINE} {
    // possible end of heredoc (depending on label)

    // determine supposed end label (and if there is a semicolon or not)
    String supposedLabel = text().trim();
    boolean semicolon = false;
    if (supposedLabel.charAt(supposedLabel.length() - 1) == ';') {
        semicolon = true;
        supposedLabel = supposedLabel.substring(0, supposedLabel.length() - 1);
    }

    if (supposedLabel.equals(this.heredocLabel)) {
        // the end label matches the start label

        if (semicolon) {
            yypushback(length() - supposedLabel.length());
        }
       
		yybegin(ST_IN_SCRIPTING);
        return symbol(Symbols.T_END_HEREDOC, "T_END_HEREDOC");

    } else {
        // the end label doesn't match the start label
        return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
    }

}

<ST_HEREDOC>^{ANY_CHAR}+{NEWLINE} {
        return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
}

<ST_SINGLE_QUOTE>([^'\\]|\\[^'\\])+ {
    return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
}


//<ST_DOUBLE_QUOTES>[`]+ {
//    return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
//}


<ST_BACKQUOTE>[\"]+ {
    return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
}

<ST_BACKQUOTE,ST_HEREDOC>"$"[^a-zA-Z_\x7f-\xbb\xbc-\xde\xdf-\xff{] {
	if (length() == 2) {
		yypushback(1);
	}
    return symbol(Symbols.T_STRING, "T_STRING");
}

// NJ: split up rule for {ENCAPSED_TOKENS} since CUP doesn't support character tokens
<ST_BACKQUOTE,ST_HEREDOC> {

    "[" { return symbol(Symbols.T_OPEN_RECT_BRACES, "T_OPEN_RECT_BRACES"); }
    "]" { return symbol(Symbols.T_CLOSE_RECT_BRACES, "T_CLOSE_RECT_BRACES"); }
    "{" { return symbol(Symbols.T_OPEN_CURLY_BRACES, "T_OPEN_CURLY_BRACES"); }
    "}" { return symbol(Symbols.T_CLOSE_CURLY_BRACES, "T_CLOSE_CURLY_BRACES"); }
    "$" { return symbol(Symbols.T_DOLLAR, "T_DOLLAR"); }

}

<ST_SINGLE_QUOTE>"\\'" {
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_SINGLE_QUOTE>"\\\\" {
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_BACKQUOTE>"\\`" {
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_BACKQUOTE,ST_HEREDOC>"\\"[0-7]{1,3} {
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_BACKQUOTE,ST_HEREDOC>"\\x"[0-9A-Fa-f]{1,2} {
    return symbol(Symbols.T_STRING, "T_STRING");
}

<ST_HEREDOC>[\"'`]+ {
    return symbol(Symbols.T_ENCAPSED_AND_WHITESPACE, "T_ENCAPSED_AND_WHITESPACE");
}


<ST_BACKQUOTE>[`] {
    yybegin(ST_IN_SCRIPTING);
    return symbol(Symbols.T_BACKTICK, "T_BACKTICK");
}

<ST_SINGLE_QUOTE>['] {
    yybegin(ST_IN_SCRIPTING);
    return symbol(Symbols.T_DOUBLE_QUOTE, "T_DOUBLE_QUOTE");
}

<ST_BACKQUOTE,YYINITIAL,ST_IN_SCRIPTING,ST_LOOKING_FOR_PROPERTY><<EOF>> {
    return null;
}

<ST_COMMENT><<EOF>> {
    cleanMore();
    return null;
}

<ST_IN_SCRIPTING,ST_DOUBLE_QUOTES,ST_SINGLE_QUOTE,ST_BACKQUOTE,ST_HEREDOC,ST_LOOKING_FOR_PROPERTY,ST_LOOKING_FOR_VARNAME,ST_VAR_OFFSET,ST_COMMENT,ST_ONE_LINE_COMMENT> {ANY_CHAR} {
    System.err.println("read ANY_CHAR at wrong place (state="+yystate()+"):");
    System.err.println("line " + yyline + ", column " + yycolumn);
    System.err.println("character: " + text());
    return null;
}
