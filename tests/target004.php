<?php
$a = new StdClass;

$a+1; // type error


$a = "str";

$a[] = "a"; // type error

$a = "str";
echo $a[3]; // out of bounds


// option
$a = array(1, 2, 3);
echo $a[3]; // out of bounds/undefined index
