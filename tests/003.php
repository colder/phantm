<?php
class A {
    const FOO = 2;
    const PLOP = self::FOO;
    public $v1;
    public $v2 = 1;
    protected $v3 = self::PLOP;
    private $v4;
    private static $v5;

    public function f1() {

    }

    protected function f2() {

    }

    private function f3() {

    }

    final function f4() {

    }

    static function f5() {

    }
}

$a = new A;
$a = &new A;

A::FOO;
A::$v5;

$b = clone $a;
