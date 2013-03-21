/**
 * PHP 5.3 JFlex specification file
 * Based on a php4 version from Nenad Jovanovic */

package phantm.parser;

import java.util.*;

import phantm.parser.Yytoken;
import static phantm.parser.Terminals.*;

%%

%{

    private StringBuffer morePrefix;
    private boolean clearMorePrefix;

    public ArrayList<Comment> comments;

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
    public Yytoken lex() throws java.io.IOException {
        Yytoken ret = this.yylex();
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
    private Yytoken symbol(scala.Enumeration.Val type, String name) {
        return symbol(type, name, text());
    }

    private Yytoken symbol(scala.Enumeration.Val type, String name, String content) {
        // use the Symbol's "left value" as line number
        int line = yyline + 1;
        return new Yytoken(
            type,
            line,
            yycolumn,
            content);
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
%x ST_NOWDOC
%x ST_HEREDOC
%x ST_END_HEREDOC
%x ST_LOOKING_FOR_PROPERTY
%x ST_LOOKING_FOR_VARNAME
%x ST_VAR_OFFSET
%x ST_COMMENT
%x ST_ONE_LINE_COMMENT

LNUM = [0-9]+
DNUM = ([0-9]*[\.][0-9]+)|([0-9]+[\.][0-9]*)
EXPONENT_DNUM = (({LNUM}|{DNUM})[eE][+\-]?{LNUM})
HNUM = "0"[xX][0-9a-fA-F]+
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
%class Lexer
%public

%%

<ST_IN_SCRIPTING> {
    "exit" { return symbol(T_EXIT(), "T_EXIT"); }
    "die" { return symbol(T_EXIT(), "T_EXIT"); }
    "function"|"cfunction" { return symbol(T_FUNCTION(), "T_FUNCTION"); }
    "const" { return symbol(T_CONST(), "T_CONST"); }
    "return" { return symbol(T_RETURN(), "T_RETURN"); }
    "if" { return symbol(T_IF(), "T_IF"); }
    "elseif" { return symbol(T_ELSEIF(), "T_ELSEIF"); }
    "endif" { return symbol(T_ENDIF(), "T_ENDIF"); }
    "else" { return symbol(T_ELSE(), "T_ELSE"); }
    "while" { return symbol(T_WHILE(), "T_WHILE"); }
    "endwhile" { return symbol(T_ENDWHILE(), "T_ENDWHILE"); }
    "do" { return symbol(T_DO(), "T_DO"); }
    "for" { return symbol(T_FOR(), "T_FOR"); }
    "endfor" { return symbol(T_ENDFOR(), "T_ENDFOR"); }
    "foreach" { return symbol(T_FOREACH(), "T_FOREACH"); }
    "endforeach" { return symbol(T_ENDFOREACH(), "T_ENDFOREACH"); }
    "declare" { return symbol(T_DECLARE(), "T_DECLARE"); }
    "enddeclare" { return symbol(T_ENDDECLARE(), "T_ENDDECLARE"); }
    "as" { return symbol(T_AS(), "T_AS"); }
    "switch" { return symbol(T_SWITCH(), "T_SWITCH"); }
    "endswitch" { return symbol(T_ENDSWITCH(), "T_ENDSWITCH"); }
    "case" { return symbol(T_CASE(), "T_CASE"); }
    "default" { return symbol(T_DEFAULT(), "T_DEFAULT"); }
    "break" { return symbol(T_BREAK(), "T_BREAK"); }
    "continue" { return symbol(T_CONTINUE(), "T_CONTINUE"); }
    "goto" { return symbol(T_GOTO(), "T_GOTO"); }
    "echo" { return symbol(T_ECHO(), "T_ECHO"); }
    "try" { return symbol(T_TRY(), "T_TRY"); }
    "catch" { return symbol(T_CATCH(), "T_CATCH"); }
    "throw" { return symbol(T_THROW(), "T_THROW"); }
    "interface" { return symbol(T_INTERFACE(), "T_INTERFACE"); }
    "instanceof" { return symbol(T_INSTANCEOF(), "T_INSTANCEOF"); }
    "implements" { return symbol(T_IMPLEMENTS(), "T_IMPLEMENTS"); }
    "clone" { return symbol(T_CLONE(), "T_CLONE"); }
    "__halt_compiler" { return symbol(T_HALT_COMPILER(), "T_HALT_COMPILER"); }
    "abstract" { return symbol(T_ABSTRACT(), "T_ABSTRACT"); }
    "private" { return symbol(T_PRIVATE(), "T_PRIVATE"); }
    "protected" { return symbol(T_PROTECTED(), "T_PROTECTED"); }
    "public" { return symbol(T_PUBLIC(), "T_PUBLIC"); }
    "final" { return symbol(T_FINAL(), "T_FINAL"); }
    "print" { return symbol(T_PRINT(), "T_PRINT"); }
    "class" { return symbol(T_CLASS(), "T_CLASS"); }
    "namespace" { return symbol(T_NAMESPACE(), "T_NAMESPACE"); }
    "extends" { return symbol(T_EXTENDS(), "T_EXTENDS"); }
    "::" { return symbol(T_DOUBLE_COLON(), "T_DOUBLE_COLON"); }
    "new" { return symbol(T_NEW(), "T_NEW"); }
    "var" { return symbol(T_VAR(), "T_VAR"); }
    "eval" { return symbol(T_EVAL(), "T_EVAL"); }
    "include" { return symbol(T_INCLUDE(), "T_INCLUDE"); }
    "include_once" { return symbol(T_INCLUDE_ONCE(), "T_INCLUDE_ONCE"); }
    "require" { return symbol(T_REQUIRE(), "T_REQUIRE"); }
    "require_once" { return symbol(T_REQUIRE_ONCE(), "T_REQUIRE_ONCE"); }
    "use" { return symbol(T_USE(), "T_USE"); }
    "global" { return symbol(T_GLOBAL(), "T_GLOBAL"); }
    "isset" { return symbol(T_ISSET(), "T_ISSET"); }
    "empty" { return symbol(T_EMPTY(), "T_EMPTY"); }
    "static" { return symbol(T_STATIC(), "T_STATIC"); }
    "unset" { return symbol(T_UNSET(), "T_UNSET"); }
    "=>" { return symbol(T_DOUBLE_ARROW(), "T_DOUBLE_ARROW"); }
    "list" { return symbol(T_LIST(), "T_LIST"); }
    "array" { return symbol(T_ARRAY(), "T_ARRAY"); }
    "++" { return symbol(T_INC(), "T_INC"); }
    "--" { return symbol(T_DEC(), "T_DEC"); }
    "===" { return symbol(T_IS_IDENTICAL(), "T_IS_IDENTICAL"); }
    "!==" { return symbol(T_IS_NOT_IDENTICAL(), "T_IS_NOT_IDENTICAL"); }
    "==" { return symbol(T_IS_EQUAL(), "T_IS_EQUAL"); }
    "!="|"<>" { return symbol(T_IS_NOT_EQUAL(), "T_IS_NOT_EQUAL"); }
    "<=" { return symbol(T_IS_SMALLER_OR_EQUAL(), "T_IS_SMALLER_OR_EQUAL"); }
    ">=" { return symbol(T_IS_GREATER_OR_EQUAL(), "T_IS_GREATER_OR_EQUAL"); }
    "+=" { return symbol(T_PLUS_EQUAL(), "T_PLUS_EQUAL"); }
    "-=" { return symbol(T_MINUS_EQUAL(), "T_MINUS_EQUAL"); }
    "*=" { return symbol(T_MUL_EQUAL(), "T_MUL_EQUAL"); }
    "/=" { return symbol(T_DIV_EQUAL(), "T_DIV_EQUAL"); }
    ".=" { return symbol(T_CONCAT_EQUAL(), "T_CONCAT_EQUAL"); }
    "%=" { return symbol(T_MOD_EQUAL(), "T_MOD_EQUAL"); }
    "<<=" { return symbol(T_SL_EQUAL(), "T_SL_EQUAL"); }
    ">>=" { return symbol(T_SR_EQUAL(), "T_SR_EQUAL"); }
    "&=" { return symbol(T_AND_EQUAL(), "T_AND_EQUAL"); }
    "|=" { return symbol(T_OR_EQUAL(), "T_OR_EQUAL"); }
    "^=" { return symbol(T_XOR_EQUAL(), "T_XOR_EQUAL"); }
    "||" { return symbol(T_BOOLEAN_OR(), "T_BOOLEAN_OR"); }
    "&&" { return symbol(T_BOOLEAN_AND(), "T_BOOLEAN_AND"); }
    "OR" { return symbol(T_LOGICAL_OR(), "T_LOGICAL_OR"); }
    "AND" { return symbol(T_LOGICAL_AND(), "T_LOGICAL_AND"); }
    "XOR" { return symbol(T_LOGICAL_XOR(), "T_LOGICAL_XOR"); }
    "<<" { return symbol(T_SL(), "T_SL"); }
    ">>" { return symbol(T_SR(), "T_SR"); }

// Single char Tokens
    ";" { return symbol(T_SEMICOLON(), "T_SEMICOLON"); }
    ":" { return symbol(T_COLON(), "T_COLON"); }
    "," { return symbol(T_COMMA(), "T_COMMA"); }
    "." { return symbol(T_POINT(), "T_POINT"); }
    "[" { return symbol(T_OPEN_RECT_BRACES(), "T_OPEN_RECT_BRACES"); }
    "]" { return symbol(T_CLOSE_RECT_BRACES(), "T_CLOSE_RECT_BRACES"); }
    "(" { return symbol(T_OPEN_BRACES(), "T_OPEN_BRACES"); }
    ")" { return symbol(T_CLOSE_BRACES(), "T_CLOSE_BRACES"); }
    "|" { return symbol(T_BITWISE_OR(), "T_BITWISE_OR"); }
    "^" { return symbol(T_BITWISE_XOR(), "T_BITWISE_XOR"); }
    "&" { return symbol(T_BITWISE_AND(), "T_BITWISE_AND"); }
    "+" { return symbol(T_PLUS(), "T_PLUS"); }
    "-" { return symbol(T_MINUS(), "T_MINUS"); }
    "/" { return symbol(T_DIV(), "T_DIV"); }
    "*" { return symbol(T_MULT(), "T_MULT"); }
    "=" { return symbol(T_ASSIGN(), "T_ASSIGN"); }
    "%" { return symbol(T_MODULO(), "T_MODULO"); }
    "!" { return symbol(T_NOT(), "T_NOT"); }
    "~" { return symbol(T_BITWISE_NOT(), "T_BITWISE_NOT"); }
    "$" { return symbol(T_DOLLAR(), "T_DOLLAR"); }
    "<" { return symbol(T_IS_SMALLER(), "T_IS_SMALLER"); }
    ">" { return symbol(T_IS_GREATER(), "T_IS_GREATER"); }
    "?" { return symbol(T_QUESTION(), "T_QUESTION"); }
    "@" { return symbol(T_AT(), "T_AT"); }
    "\\" { return symbol(T_NS_SEPARATOR(), "T_NS_SEPARATOR"); }

    "__METHOD__" { return symbol(T_METHOD_C(), "T_METHOD_C"); }
    "__CLASS__" { return symbol(T_CLASS_C(), "T_CLASS_C"); }
    "__FUNCTION__" { return symbol(T_FUNC_C(), "T_FUNC_C"); }
    "__NAMESPACE__" { return symbol(T_FUNC_C(), "T_NS_C"); }
    "__LINE__" { return symbol(T_LINE(), "T_LINE"); }
    "__FILE__" { return symbol(T_FILE(), "T_FILE"); }
    "__DIR__" { return symbol(T_DIR(), "T_DIR"); }
    {LNUM}|{HNUM} { return symbol(T_LNUMBER(), "T_LNUMBER"); }
    {DNUM}|{EXPONENT_DNUM} { return symbol(T_DNUMBER(), "T_DNUMBER"); }
}

<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("int"|"integer"){TABS_AND_SPACES}")" { return symbol(T_INT_CAST(), "T_INT_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("real"|"double"|"float"){TABS_AND_SPACES}")" { return symbol(T_DOUBLE_CAST(), "T_DOUBLE_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"binary"{TABS_AND_SPACES}")" { return symbol(T_STRING_CAST(), "T_STRING_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"string"{TABS_AND_SPACES}")" { return symbol(T_STRING_CAST(), "T_STRING_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"array"{TABS_AND_SPACES}")" { return symbol(T_ARRAY_CAST(), "T_ARRAY_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}"object"{TABS_AND_SPACES}")" { return symbol(T_OBJECT_CAST(), "T_OBJECT_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("bool"|"boolean"){TABS_AND_SPACES}")" { return symbol(T_BOOL_CAST(), "T_BOOL_CAST"); }
<ST_IN_SCRIPTING>"("{TABS_AND_SPACES}("unset"){TABS_AND_SPACES}")" { return symbol(T_UNSET_CAST(), "T_UNSET_CAST"); }

<ST_DOUBLE_QUOTES>[\"] {
    yybegin(ST_IN_SCRIPTING);
    return symbol(T_DOUBLE_QUOTE(), "T_DOUBLE_QUOTE");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC>"{$" {
    pushState(ST_IN_SCRIPTING);
    yypushback(1);
    return symbol(T_CURLY_OPEN(), "T_CURLY_OPEN");
}

<ST_IN_SCRIPTING>[`] {
    yybegin(ST_BACKQUOTE);
    return symbol(T_BACKTICK(), "T_BACKTICK");
}

<ST_BACKQUOTE>[`] {
    yybegin(ST_IN_SCRIPTING);
    return symbol(T_BACKTICK(), "T_BACKTICK");
}


<ST_BACKQUOTE>{ANY_CHAR} {
    scanner:
    while(zzMarkedPos < zzEndRead) {
        switch(zzBuffer[zzMarkedPos-1]) {
            case '`':
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

    return symbol(T_ENCAPSED_AND_WHITESPACE(), "T_ENCAPSED_AND_WHITESPACE");
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

    return symbol(T_ENCAPSED_AND_WHITESPACE(), "T_ENCAPSED_AND_WHITESPACE");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC> "$" {LABEL} "->" {LABEL_FIRST} {
    yypushback(3);
    pushState(ST_LOOKING_FOR_PROPERTY);
    return symbol(T_VARIABLE(), "T_VARIABLE");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC> "$" {LABEL} "["  {
    yypushback(1);
    pushState(ST_VAR_OFFSET);
    return symbol(T_VARIABLE(), "T_VARIABLE");
}

<ST_IN_SCRIPTING,ST_DOUBLE_QUOTES,ST_HEREDOC,ST_BACKQUOTE,ST_VAR_OFFSET> "$" {LABEL} {
    return symbol(T_VARIABLE(), "T_VARIABLE");
}

<ST_VAR_OFFSET>"[" {
    return symbol(T_OPEN_RECT_BRACES(), "T_OPEN_RECT_BRACES");
}

<ST_VAR_OFFSET>"]" {
    popState();
    return symbol(T_CLOSE_RECT_BRACES(), "T_CLOSE_RECT_BRACES");
}

<ST_IN_SCRIPTING>"->" {
	pushState(ST_LOOKING_FOR_PROPERTY);
    return symbol(T_OBJECT_OPERATOR(), "T_OBJECT_OPERATOR");
}

<ST_LOOKING_FOR_PROPERTY>{LABEL} {
	popState();
    return symbol(T_STRING(), "T_STRING");
}

<ST_LOOKING_FOR_PROPERTY>{ANY_CHAR} {
	yypushback(length());
	popState();
}

<ST_IN_SCRIPTING>"{" {
	pushState(ST_IN_SCRIPTING);
    return symbol(T_OPEN_CURLY_BRACES(), "T_OPEN_CURLY_BRACES");
}

<ST_DOUBLE_QUOTES,ST_BACKQUOTE,ST_HEREDOC>"${" {
	pushState(ST_LOOKING_FOR_VARNAME);
    return symbol(T_DOLLAR_OPEN_CURLY_BRACES(), "T_DOLLAR_OPEN_CURLY_BRACES");
}

<ST_IN_SCRIPTING>"}" {
    // TODO: could make problems
	// if (yy_start_stack_ptr) {
	//	yy_pop_state();
	// }
    
//    System.out.println("POPPING STATE!!!");
    popState();
    return symbol(T_CLOSE_CURLY_BRACES(), "T_CLOSE_CURLY_BRACES");
}

<ST_LOOKING_FOR_VARNAME>{LABEL} {
	popState();
	pushState(ST_IN_SCRIPTING);
    return symbol(T_STRING_VARNAME(), "T_STRING_VARNAME");
}

<ST_LOOKING_FOR_VARNAME>{ANY_CHAR} {
	yypushback(length());
	popState();
	pushState(ST_IN_SCRIPTING);
}

<ST_VAR_OFFSET>{LNUM}|{HNUM} {
    return symbol(T_NUM_STRING(), "T_NUM_STRING");
}

// <YYINITIAL>(([^<]|"<"[^?%s<]){1,400})|"<s"|"<" {
<YYINITIAL>(([^<]|"<"[^?%s<])*)|"<s"|"<" {
    // NJ: replaced {1,400} by * (because it's faster)
    return symbol(T_INLINE_HTML(), "T_INLINE_HTML");
}

<YYINITIAL>"<?"|"<script"{WHITESPACE}+"language"{WHITESPACE}*"="{WHITESPACE}*("php"|"\"php\""|"\'php\'"){WHITESPACE}*">" {
    yybegin(ST_IN_SCRIPTING);
//    return symbol(T_OPEN_TAG(), "T_OPEN_TAG");
}

<YYINITIAL>"<%="|"<?=" {
    yybegin(ST_IN_SCRIPTING);
    //return new Yytoken("T_ECHO", text());
//    return symbol(T_OPEN_TAG_WITH_ECHO(), "T_OPEN_TAG_WITH_ECHO");
}

<YYINITIAL>"<%" {
    yybegin(ST_IN_SCRIPTING);
//    return symbol(T_OPEN_TAG(), "T_OPEN_TAG");
}

<YYINITIAL>"<?php"([ \t]|{NEWLINE}) {
	yybegin(ST_IN_SCRIPTING);
//    return symbol(T_OPEN_TAG(), "T_OPEN_TAG");
}


<ST_IN_SCRIPTING,ST_VAR_OFFSET>{LABEL} {
    return symbol(T_STRING(), "T_STRING");
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
    return symbol(T_SEMICOLON(), "T_SEMICOLON");
}


<ST_IN_SCRIPTING>"%>"{NEWLINE}? {
    yybegin(YYINITIAL);
    return symbol(T_SEMICOLON(), "T_SEMICOLON");
}

<ST_IN_SCRIPTING>b?([\"]([^$\"\\]|("\\".))*[\"]) {
    return symbol(T_CONSTANT_ENCAPSED_STRING(), "T_CONSTANT_ENCAPSED_STRING", text());
}

<ST_IN_SCRIPTING>b?([']([^'\\]|("\\".))*[']) {
    return symbol(T_CONSTANT_ENCAPSED_STRING(), "T_CONSTANT_ENCAPSED_STRING", text());
}

<ST_IN_SCRIPTING>b?[\"] {
    yybegin(ST_DOUBLE_QUOTES);
    return symbol(T_DOUBLE_QUOTE(), "T_DOUBLE_QUOTE");
}

<ST_IN_SCRIPTING>b?"<<<"{TABS_AND_SPACES}({LABEL}|([']{LABEL}['])|([\"]{LABEL}[\"])){NEWLINE} {
    // start of heredoc/nowdoc
    int initPos = zzStartRead;
    int pos     = zzStartRead;
    int labelLength = length();

    if (text().startsWith("b")) {
        pos++;
        labelLength--;
    }

    // skip <<<
    pos += 3;

    if (text().endsWith("\r\n")) {
        labelLength -= 5; // <<< and \r\n
    } else {
        labelLength -= 4; // <<< and \n or \r
    }

    // skip spaces and tabs
    while(zzBuffer[pos] == ' ' || zzBuffer[pos] == '\t') {
        pos++;
        labelLength--;
    }

    if (zzBuffer[pos] == '\'') {
        pos++;
        yybegin(ST_NOWDOC);
        labelLength -= 2;
    } else {
        if (zzBuffer[pos] == '\"') {
            pos++;
            labelLength -= 2;
        }
        yybegin(ST_HEREDOC);
    }

    this.heredocLabel = text().substring((pos-initPos), (pos-initPos) + labelLength);

    // Check if it's directly in the next line
    if (labelLength < zzEndRead - zzMarkedPos) {
        int lpos = zzMarkedPos;

        String label = "";
        while(lpos < zzEndRead && isLabelStart(zzBuffer[lpos]) && lpos-zzMarkedPos <= labelLength) {
            label = label + zzBuffer[lpos];
            lpos++;
        }

        if (label.equals(this.heredocLabel)) {
            if (zzBuffer[lpos] == ';') {
                lpos++;
            }

            if (zzBuffer[lpos] == '\n' || zzBuffer[lpos] == '\r') {
                yybegin(ST_END_HEREDOC);
            }
        }

    }

    return symbol(T_START_HEREDOC(), "T_START_HEREDOC");
}


<ST_END_HEREDOC>{ANY_CHAR} {
    zzMarkedPos += this.heredocLabel.length()-1;
    yybegin(ST_IN_SCRIPTING);
    return symbol(T_END_HEREDOC(), "T_END_HEREDOC");
}

<ST_IN_SCRIPTING>b?['] {
    yybegin(ST_SINGLE_QUOTE);
    return symbol(T_DOUBLE_QUOTE(), "T_DOUBLE_QUOTE");
}

<ST_NOWDOC>{ANY_CHAR} {
    if (zzMarkedPos > zzEndRead) {
        return null;
    }

    zzMarkedPos--;

    scanner:
    while(zzMarkedPos < zzEndRead) {
        switch(zzBuffer[zzMarkedPos++]) {
            case '\r':
                if (zzBuffer[zzMarkedPos] == '\n') {
                    zzMarkedPos++;
                }
                /* fall through */
            case '\n':
                /* Check for ending label on the next line */
                int pos = zzMarkedPos;
                String label = "";
                while(pos < zzEndRead && isLabelStart(zzBuffer[pos])) {
                    label = label + zzBuffer[pos];
                    pos++;
                }

                if (label.equals(this.heredocLabel)) {
                    if (pos < zzEndRead && zzBuffer[pos] == ';') {
                        pos++;
                    }

                    if (zzBuffer[pos] == '\r' || zzBuffer[pos] == '\n') {
                        yybegin(ST_END_HEREDOC);
                        break scanner;
                    }
                }
                zzMarkedPos = pos;
            default:
                continue;
        }
    }
    return symbol(T_ENCAPSED_AND_WHITESPACE(), "T_ENCAPSED_AND_WHITESPACE");
}

<ST_HEREDOC>{ANY_CHAR} {
    if (zzMarkedPos > zzEndRead) {
        return null;
    }

    zzMarkedPos--;

    scanner:
    while(zzMarkedPos < zzEndRead) {
        switch(zzBuffer[zzMarkedPos++]) {
            case '\r':
                if (zzBuffer[zzMarkedPos] == '\n') {
                    zzMarkedPos++;
                }
                /* fall through */
            case '\n':
                /* Check for ending label on the next line */
                String label = "";
                int pos = zzMarkedPos;
                while(pos < zzEndRead && isLabelStart(zzBuffer[pos])) {
                    label = label + zzBuffer[pos];
                    pos++;
                }

                if (label.equals(this.heredocLabel)) {
                    if (pos < zzEndRead && zzBuffer[pos] == ';') {
                        pos++;
                    }

                    if (zzBuffer[pos] == '\r' || zzBuffer[pos] == '\n') {
                        yybegin(ST_END_HEREDOC);
                        break scanner;
                    }
                }
                zzMarkedPos = pos;

                continue;
            case '$':
                if (isLabelStart(zzBuffer[zzMarkedPos]) || zzBuffer[zzMarkedPos] == '{') {
                    break;
                }
                continue;
            case '{':
                if (zzBuffer[zzMarkedPos] == '$') {
                    break;
                }
                continue;
            case '\\':
                if (zzMarkedPos < zzEndRead && zzBuffer[zzMarkedPos] != '\n' && zzBuffer[zzMarkedPos] != '\r') {
                    zzMarkedPos++;
                }
                /* fall through */
            default:
                continue;
        }

        zzMarkedPos--;
        break;
    }
    return symbol(T_ENCAPSED_AND_WHITESPACE(), "T_ENCAPSED_AND_WHITESPACE");
}

<ST_SINGLE_QUOTE>([^'\\]|\\[^'\\])+ {
    return symbol(T_ENCAPSED_AND_WHITESPACE(), "T_ENCAPSED_AND_WHITESPACE");
}


//<ST_DOUBLE_QUOTES>[`]+ {
//    return symbol(T_ENCAPSED_AND_WHITESPACE(), "T_ENCAPSED_AND_WHITESPACE");
//}


// NJ: split up rule for {ENCAPSED_TOKENS} since CUP doesn't support character tokens
<ST_SINGLE_QUOTE>"\\'" {
    return symbol(T_STRING(), "T_STRING");
}

<ST_SINGLE_QUOTE>"\\\\" {
    return symbol(T_STRING(), "T_STRING");
}

<ST_SINGLE_QUOTE>['] {
    yybegin(ST_IN_SCRIPTING);
    return symbol(T_DOUBLE_QUOTE(), "T_DOUBLE_QUOTE");
}

/*
<ST_BACKQUOTE,YYINITIAL,ST_IN_SCRIPTING,ST_LOOKING_FOR_PROPERTY><<EOF>> {
    return null;
}

<ST_COMMENT><<EOF>> {
    cleanMore();
    return null;
}
*/

<ST_IN_SCRIPTING,ST_DOUBLE_QUOTES,ST_SINGLE_QUOTE,ST_BACKQUOTE,ST_HEREDOC,ST_LOOKING_FOR_PROPERTY,ST_LOOKING_FOR_VARNAME,ST_VAR_OFFSET,ST_COMMENT,ST_ONE_LINE_COMMENT> {ANY_CHAR} {
    System.err.println("read ANY_CHAR at wrong place (state="+yystate()+"):");
    System.err.println("line " + yyline + ", column " + yycolumn);
    System.err.println("character: " + text());
    return null;
}
