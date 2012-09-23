<?php

namespace foo;

use SplFileInfo;
use big\blah\boondoggle;
use \Exception as Ex;

/**
 * Simple type inferences
 */

if (isset($_GET['id'])) {
    $a = (int)$_GET['id'];
} else {
    $a = array();
}

if ($c = is_int($a)) {
        $b = $a/2;
            echo $b + 2;
}

namespace bah;

function fiddle() {

}

namespace rum\diddle;

class echos {

}

fiddler();

\whoknows\funcme();

blah\func3();

bah\fiddle();

foo\bah\fiddle();

\foo\bar\fiddle();