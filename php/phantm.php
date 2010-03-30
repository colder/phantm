<?php
/**
 * Phantm
 *
 * Dumps the state of all variables, so that it can be precisely imported
 * into phantm
 *
 * Call it like that: phantm_dumpanddie(get_defined_vars());
 */
function phantm_dumpanddie(array $vars) {
    $bt = debug_backtrace();
    $file = $bt[0]['file'];
    $line = $bt[0]['line'];

    $path = dirname(__FILE__)."/".basename($file)."--".date('d-m-y--H\hi\ms').".dump";
    $fh = fopen($path, "w");
    fwrite($fh, "# Dumped ".$file." at line ".$line." \n");
    fwrite($fh, "# Date: ".date("r")."\n");
    fwrite($fh, serialize($vars));
    fclose($fh);

    copy($path, dirname(__FILE__)."/last.dump");

    exit("\n--- phantm: Done recording state, shutting down ---");
}
