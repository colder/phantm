<?php
/**
 * Simple type inferences
 */

if (isset($_GET['id'])) {
    $a = (int)$_GET['id'];
} else {
    $a = array();
}

if (is_int($a)) {
        $b = $a/2;
            echo $b + 2;
}

