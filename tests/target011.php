<?php
class A {}
class B extends A {}
class C {}

function foo(A $a) {

}

foo(new B); // ok
foo(new C); // type error
