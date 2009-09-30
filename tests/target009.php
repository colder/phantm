<?php
class A implements Iterator {
    public function valid() {
        return true;
    }

    public function next() { }

    public function rewind() { }

    public function key() {
        return 2;
    }
    public function current() {
        return 2;
    }
}


$a = new A;

foreach($a as $v) {
    $v[0] = 1; // type error, $v is an int
}
