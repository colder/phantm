<?php
class A{}
$a = new A;
$b = new A;


if (true) {
    $c = $a;
} else {
    $c = $b;
}

$c->a = 2;

echo $a->a;
