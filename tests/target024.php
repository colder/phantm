<?php

class A {
    public $i = 2;
    public $s = "asd";
}
$plop = "asd";
$a = new A;
$a->$plop = 2;

$c = new A;

$b = new A;
$b->foo = 2;

if (2 > 3) {
    $a = $b;
    $c = $b;
}

echo $a->foo;
echo $c->foo;
