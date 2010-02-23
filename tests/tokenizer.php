<?php
$tokens_chars = array(
    ',' => 'T_COMMA',
    '=' => 'T_ASSIGN',
    '?' => 'T_QUESTION',
    ':' => 'T_COLON',
    '|' => 'T_BITWISE_OR',
    '^' => 'T_BITWISE_XOR',
    '&' => 'T_BITWISE_AND',
    '<' => 'T_IS_SMALLER',
    '>' => 'T_IS_GREATER',
    '+' => 'T_PLUS',
    '-' => 'T_MINUS',
    '.' => 'T_POINT',
    '*' => 'T_MULT',
    '/' => 'T_DIV',
    '%' => 'T_MODULO',
    '!' => 'T_NOT',
    '~' => 'T_BITWISE_NOT',
    '@' => 'T_AT',
    '[' => 'T_OPEN_RECT_BRACES',
    '{' => 'T_OPEN_CURLY_BRACES',
    '}' => 'T_CLOSE_CURLY_BRACES',
    '(' => 'T_OPEN_BRACES',
    ')' => 'T_CLOSE_BRACES',
    ';' => 'T_SEMICOLON',
    '$' => 'T_DOLLAR',
    '`' => 'T_BACKTICK',
    '"' => 'T_DOUBLE_QUOTE',
    '\'' => 'T_SINGLE_QUOTE',
    ']' => 'T_CLOSE_RECT_BRACES',
);

foreach(token_get_all(file_get_contents($argv[1])) as $t) {
    if (is_array($t)) {
        if (token_name($t[0]) == "T_WHITESPACE") continue;
        if (token_name($t[0]) == "T_OPEN_TAG") continue;
        if (token_name($t[0]) == "T_OPEN_TAG_WITH_ECHO") continue;

        if (token_name($t[0]) == "T_PAAMAYIM_NEKUDOTAYIM") {
            echo "T_DOUBLE_COLON($t[1])\n";
        } else {
            echo token_name($t[0])."($t[1])\n";
        }
        //echo token_name($t[0])."\n";
    } else {
        echo $tokens_chars[$t]."($t)\n";
        //echo $tokens_chars[$t]."\n";
    }
}
