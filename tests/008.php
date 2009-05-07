<?php

class A {

    public function B() {
        class C {}
    }
}


function test() {
    class D {}
}

if (1) {
    class E {}
}

while(1) {
    class F {}
}

foreach(array() as $f) {
    class G {}
}


