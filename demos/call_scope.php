<?php


class scoping_test {
    public static function foo() {
        self::bah();          //should pass
        self::guh();          //should pass
    }

  protected static function guh() {
  }

   private static function bah() {
   }

   function aah() {
      $this->blah();          //should pass
   }

   private function blah() {
   }

   protected function buh() {
   }
}

class scoping_test_child extends scoping_test {
     function tse() {
        $this->aah();       // should pass
        $this->buh();      // should pass

        $this->blah();    // should fail
        static::bah();   // should fail
        $this->bah();    // should fail
        scoping_test::bah();    // should fail

        scoping_test::guh(); // should pass
        static::guh(); // should pass
        $this->guh(); //should pass
     }
}

scoping_test::foo();                // should pass
scoping_test::bah();               // should fail

$st  = new scoping_test_child();
$st->tse();                              // should pass