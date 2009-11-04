<?php
$a = new StdClass;

// type error
$a+1;


$a = "str";

$a[] = "a"; // type error

$a = "str";
echo $a[3]; // out of bounds


// option
$a = array(1, 2, 3);
$a[3]; // out of bounds/undefined index
