<?php
class a {
    public function plop() {}
}
$a = new a;
$a->bar(); // undefined function

class a2 {
    public function plop(){}

    public function __call($n, $a){}
}
$a2 = new a2;
$a2->bar(); // ok
