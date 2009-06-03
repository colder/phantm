<?php
class A {
    public static $c = 2;
    public $a = 1;
    private $d = 3;
    const TEST = 2;

    function asdf ($a1, $a2, $a3) {

    }
}


class B extends A {
    public static $b = 2;
    public static $d = 2;
    const TEST = 4;

    function asdf ($b1, $b2, $b3) {
        $a = 2 + $b1;
        $c = "asd".$a;
        $d = array();
        $e = "asd".$d;
        return $e;
    }

    function asdf2 ($c1, $c2, $c3) {

    }
}

$b = new B; 

var_dump(B::$d);
var_dump(B::$a);


