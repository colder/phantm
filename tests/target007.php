<?php
$a = "asd";
// type error, $a is not iterable
foreach($a as $plop);

$a = array(new StdClass, new StdClass, new StdClass, 2);
// type error, $a contains objects only
foreach($a as $v) $v+2;
