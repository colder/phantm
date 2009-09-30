<?php
$a = "asd";
foreach($a as $plop); // type error, $a is not iterable



$a = array(new StdClass, new StdClass, new StdClass);

foreach($a as $v) $v+2; // type error, $a contains objects only
