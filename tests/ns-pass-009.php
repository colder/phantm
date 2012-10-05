<?php
namespace A\B\C;

/* This function is A\B\C\fopen */
function fopen() {
         /* ... */
     $f = \fopen("/tmp/plop", "r"); // call global fopen
      return $f;
}
