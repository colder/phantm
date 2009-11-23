<?php
$fh = fopen('internal_functions.txt', 'r');
$ftoh = fopen('internal_functions.xml', 'w');

fwrite($ftoh, "<functions>\n");
while($fh && !feof($fh)) {
    $line = fgets($fh);
    if ($line[0] == '#') continue;
    
    $proto = trim($line);
    if ($proto != "") {
        if (preg_match("#^(\w+(?:\|\w+)*)\s*([:\w]+)s*\((.*)\)#", $proto, $match)) {

            fwrite($ftoh, " <function name=\"$match[2]\">\n");
            fwrite($ftoh, "  <return><type>".implode("</type><type>", explode("|", $match[1]))."</type></return>\n");

            // match args

            $args_raw = explode(",", $match[3]);

            $args = array();
            $isOPT = false;
            fwrite($ftoh,  "  <args>\n");
            if (trim($match[3]) != "" && trim($match[3]) != "void") {
                foreach($args_raw as $arg) {
                    $arg = trim($arg);


                    if ($arg[0] == "[") {
                        $isOPT++;
                    }

                    $types = array("mixed");

                    if (preg_match("/^\[?\s*(\w+(?:\|\w+)*)\s+/", $arg, $match)) {
                        $types = explode("|", $match[1]);
                    } else {
                        echo "Can't match types!: $arg\n";
                    }

                    fwrite($ftoh, "   <arg opt=\"".(int)($isOPT > 0)."\"><type>".implode("</type><type>", $types)."</type></arg>\n");

                    if ($arg[strlen($arg)-1] == "]") {
                        $isOPT--;
                    }

                    $args[] = $arg_infos;

                }
            }
            fwrite($ftoh, "  </args>\n </function>\n\n");

        } else {
            echo "Proto \"$proto\" failed to match: \n";
            continue;
        }
    }
}

fwrite($ftoh,  "</functions>\n");
