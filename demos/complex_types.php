<?php
$ppl = array(
    array("name" => "John",
          "age" => 10,
          "kids" => array("Paul", "Nick")),
    array("name" => "Jack",
          "age" => 40,
          "kids" => array("Anna")),
);

$last = null;
foreach($ppl as $p) {
        $last = $p;
}

if ($last) {
    echo $last['name']." is ".$last['age']." years old and has ".count($last["age"])." kids!";
}
