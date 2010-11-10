<?php
/** 
 * @param $a array
 * @param $k int|string
 * @return any
 */
function find($a, $k) {
   if (isset($a[$k])) {
      return $a[$k];
   }
   return false;
}

$array = array(
    "foo" => array(1,2),
    "bar" => 5
);

if (isset($_GET['id'])) {
    $i = (int)$_GET['id'];
} else {
    $i = 0;
}

if ($i >= 2) {
    $i = null;
}

if ($i !== null) {
    $a = find($array, "foo");
    echo $a[$i];
}
