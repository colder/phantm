<?php
/*
 * @param string $file
 */
function bzfile($file) { 
    $bz = bzopen($file, "r");
    $str = "";
    while (!feof($bz)) {
        $str = $str . bzread($bz,8192); bzclose($bz);
    }
    return $str;
}
