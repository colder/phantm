<?php

$content = file_get_contents('/home/colder/cvs/php5.3/Zend/zend_language_parser.y');


$common = '{: RESULT = createRuleNode(CUP$PhpParser$stack, CUP$PhpParser$top, prodNumber, prodName, prodLength); :}';
$tokens = array(
    ',' =>   'T_COMMA',
    '=' =>   'T_ASSIGN',
    '?' =>   'T_QUESTION',
    ':' =>   'T_COLON',
    '|' =>   'T_BITWISE_OR',
    '^' =>   'T_BITWISE_XOR',
    '&' =>   'T_BITWISE_AND',
    '<' =>   'T_IS_SMALLER',
    '>' =>   'T_IS_GREATER',
    '+' =>   'T_PLUS',
    '-' =>   'T_MINUS',
    '.' =>   'T_POINT',
    '*' =>   'T_MULT',
    '/' =>   'T_DIV',
    '%' =>   'T_MODULO',
    '!' =>   'T_NOT',
    '~' =>   'T_BITWISE_NOT',
    '@' =>   'T_AT',
    '[' =>   'T_OPEN_RECT_BRACES',
    '{' =>   'T_OPEN_CURLY_BRACES',
    '}' =>   'T_CLOSE_CURLY_BRACES',
    '(' =>   'T_OPEN_BRACES',
    ')' =>   'T_CLOSE_BRACES',
    ';' =>   'T_SEMICOLON',
    '$' =>   'T_DOLLAR',
    '`' =>   'T_BACKTICK',
    '"' =>   'T_DOUBLE_QUOTE',
    '\\\'' =>   'T_SINGLE_QUOTE',
    ']' =>   'T_CLOSE_RECT_BRACES',
);
$replace = array();
foreach ($tokens as $k => $t) {
    $replace["'$k'"] = $t;
}

preg_match_all('#^(\w+):.*\n([\w\W]+)^;#mU', $content, $matches);

foreach ($matches[2] as $key => $production) {
    // replace explicit tokens
    $production = strtr($production, $replace);
    // remove rules
    $production = trim(preg_replace("/\{[^}]+\}/", "", $production));
    // split OR's
    $rules = array();
    foreach (preg_split('/\s+\|\s+/', $production) as $or) {
        $rules[] = "$or $common";
    }

    echo "non terminal ParseNode ".$matches[1][$key].";\n";
//    echo "\n".$matches[1][$key]." ::= \n";
//    echo "      ".implode("\n    | ", $rules);
//    echo "\n;\n";
}
