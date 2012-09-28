<?php

$fh = fopen("path", "rw");

if ($fh) {
    echo fread($fh, 40);
}

$req = mysql_query("SELECT * FROM `foo`");

if ($req) {
    while($r = mysql_fetch_row($req)) {
        echo count($r);
    }
}

$x = new Exception;