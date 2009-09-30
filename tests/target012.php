<?php
class A {
    public function foo() {
        return 2;
    }
}


class B extends A {
    public function foo() {
        return "a";
    }
}


$b = new B;

$a = new A;

$b->foo();
$a->foo();
