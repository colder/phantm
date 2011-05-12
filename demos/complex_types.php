<?php
$ppl = array(
    array("name" => "John",
          "age" => 10),
    array("name" => "Jack",
          "age" => 40)
);

$last = null;
foreach($ppl as $p) {
        $last = $p;
}

if ($last) {
    echo $last['name']." is ".$last['agE']." years old!";
}
