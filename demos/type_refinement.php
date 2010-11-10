<?php
if (2+2 > 3) {
    $a = 2;
} else {
    $a = null;
}

// Error, but refine to Int(2)
$b = $a + 2;

// No error
$b = $a + 4;


