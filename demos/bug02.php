<?php
// inplicit conversions
 /*
  * @param bool $a
  */
function foo($a) {
    if ($a) {
        echo "yes!";
    } else {
        echo "no!";
    }
}

foo('true');
foo('false');
