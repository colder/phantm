<?php

namespace foo;

use SplFileInfo;
use big\blah\boondoggle;
use SplFileObject as SFO;

$a = new \Exception;                    // pass absolute
$x = new SplFileInfo("a");      // pass use
$e = new SFO();                              // pass alias
$xxx = SFO::READ_AHEAD;              // pass alias
$y = isset($x);
$z = is_int(3);
$a = empty($a);
$b = count(array());

bah\fiddle();                              // pass - current

namespace bah;

function fiddle() {}

fiddle();                                   // pass current

namespace rum\diddle;

class echos {}

$xxx = new echos(); // pass - current

fiddler(); // fail -- unknown

\whoknows\funcme(); // fail - unknown, global

blah\func3(); // fail - unknown, current

bah\fiddle(); // fail - current

foo\bah\fiddle(); // fail - current

\foo\bah\fiddle(); // pass - global

$xx = new echos(); // fail - current

$xxxx = new \foo\bah\rum\diddle\echos();