<?php

class A implements arrayaccess {
    function offsetSet($n, $v) {

    }
//...
}

$a = new a;

$a[0] = 2; // ok


$a = new StdClass;
$a[0] = 2; // error
