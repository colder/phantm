<?php
$a = array(0 => 1, 1 => 2, 2 => 3, 3 => "string");

// error, array $a doesn't contain objects
echo $a[0];
echo $a[0]->plop;
