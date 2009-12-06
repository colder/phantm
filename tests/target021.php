<?php
class A{}
$a = new A;
$b = new A;


if (1+2 > 2) {
    $c = $a;
} else {
    $c = $b;
    $c->a = 2;
}

$c->a = 2;

echo $a->a;
