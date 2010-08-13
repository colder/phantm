<?php
/**
 * Phantm
 *
 * Dumps the state of all variables, so that it can be precisely imported
 * into phantm
 *
 * Call it like that: phantm_dumpanddie(get_defined_vars());
 * @param string $path
 * @return string
 */
function phantm_incl($path) {

    if (isset($GLOBALS['__phantm_fh_path'])) {
        $p = $GLOBALS['__phantm_fh_path'];
    } else {
        $p = basename($_SERVER['SCRIPT_FILENAME'])."--".date('d-m-y--H\hi\ms').".incl";
        $GLOBALS['__phantm_fh_path'] = $p;
    }

    $fh = fopen($p, "a");

    $bt = debug_backtrace();
    $file = $bt[0]['file'];
    $line = $bt[0]['line'];

    fwrite($fh, strlen($file).":".$file.":".$line.":".strlen($path).":".$path."\n");

    fclose($fh);

    copy($p, "last.incl");

    return $path;
}

function phantm_dumpanddie(array $vars) {

    $bt = debug_backtrace();
    $file = $bt[0]['file'];
    $line = $bt[0]['line'];

    // remap global entries to superglobals
    $vars['GLOBALS']['GLOBALS']  = &$vars['GLOBALS'];
    $vars['GLOBALS']['_GET']     = &$vars['_GET'];
    $vars['GLOBALS']['_POST']    = &$vars['_POST'];
    $vars['GLOBALS']['_REQUEST'] = &$vars['_REQUEST'];
    $vars['GLOBALS']['_COOKIE']  = &$vars['_COOKIE'];
    $vars['GLOBALS']['_SESSION'] = &$vars['_SESSION'];
    $vars['GLOBALS']['_FILES']   = &$vars['_FILES'];
    $vars['GLOBALS']['_ENV']     = &$vars['_ENV'];
    $vars['GLOBALS']['_SERVER']  = &$vars['_SERVER'];

    if (ini_get('register_long_arrays')) {
        $vars['HTTP_GET_VARS']       = &$vars['_GET'];
        $vars['HTTP_POST_VARS']      = &$vars['_POST'];
        $vars['HTTP_ENV_VARS']       = &$vars['_ENV'];
        $vars['HTTP_COOKIE_VARS']    = &$vars['_COOKIE'];
        $vars['HTTP_SERVER_VARS']    = &$vars['_SERVER'];
        $vars['GLOBALS']['HTTP_GET_VARS']       = &$vars['_GET'];
        $vars['GLOBALS']['HTTP_POST_VARS']      = &$vars['_POST'];
        $vars['GLOBALS']['HTTP_ENV_VARS']       = &$vars['_ENV'];
        $vars['GLOBALS']['HTTP_COOKIE_VARS']    = &$vars['_COOKIE'];
        $vars['GLOBALS']['HTTP_SERVER_VARS']    = &$vars['_SERVER'];
    }
    $path = basename(basename($_SERVER['SCRIPT_FILENAME']))."--".date('d-m-y--H\hi\ms').".dump";
    $fh = fopen($path, "w");

    if (!$fh) die("Failed to write dump...");
    fwrite($fh, "# Dumped state of ".$file." at line ".$line."  \n");
    fwrite($fh, "# Date: ".date("r")."\n");
    fwrite($fh, "# Included files:\n");
    $files = get_included_files();

    foreach ($files as $f) {
        $f = realpath($f);
        if (($f === __FILE__) || ($f === $file)) continue;

        fwrite($fh, $f."\n");
    }

    fwrite($fh, "# Function declarations:\n");

    $funcs = get_defined_functions();
    foreach ($funcs['user'] as $f) {
        if ($f == 'phantm_incl' || $f == 'phantm_dumpanddie') continue;

        $rf = new ReflectionFunction($f);

        fwrite($fh, $f.":".$rf->getStartLine().":".$rf->getFileName()."\n");
    }

    fwrite($fh, "# Classes declarations:\n");
    $classes = get_declared_classes();
    foreach ($classes as $c) {
        $rc = new ReflectionClass($c);

        if (!$rc->isUserDefined()) continue;

        fwrite($fh, $c.":".$rc->getStartLine().":".$rc->getFileName()."\n");
    }


    fwrite($fh, "# Constants:\n");
    $allConsts = get_defined_constants(true);

    $consts = !empty($allConsts['user']) ? $allConsts['user'] : array();
    fwrite($fh, strtr(serialize($consts), array("\\" => "\\\\", "\n" => "\\n", "\r" => "\\r"))."\n");

    fwrite($fh, "# Heap state:\n");
    unset($vars['GLOBALS']);
    fwrite($fh, strtr(serialize($vars), array("\\" => "\\\\", "\n" => "\\n", "\r" => "\\r"))."\n");
    fclose($fh);

    copy($path, "last.dump");

    exit("\n--- phantm: Done recording state to ".$path.", shutting down ---\n");
}
