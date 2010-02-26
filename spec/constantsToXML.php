<?php
foreach(get_defined_constants(true) as $name => $csts) { 

    echo "<!-- $name -->\n";
    foreach($csts as $name => $v) {
        $type = gettype($v);
        echo "<constant name=\"$name\"><type name=\"$type\" /></constant>\n";
    }
}
