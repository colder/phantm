<?php
foreach(get_declared_classes() as $c) {
    $rc = new ReflectionClass($c);
    echo "  <class name=\"$c\">\n";
    echo "   <fields>\n";
    foreach($rc->getProperties() as $p) {
        if ($p->isStatic()) continue;

        if ($p->isPrivate()) {
            $vis = "private";
            $typ = "any";
        } else if ($p->isProtected()) {
            $vis = "protected";
            $typ = "any";
        } else {
            $vis = "public";
            $typ = gettype($p->getValue());
        }

        echo "    <field name=\"$p->name\" visibility=\"$vis\"><type name=\"$typ\" /></field>\n";
    }
    echo "   </fields>\n";
    echo "   <staticfields>\n";
    foreach($rc->getProperties() as $p) {
        if (!$p->isStatic()) continue;

        if ($p->isPrivate()) {
            $vis = "private";
            $typ = "any";
        } else if ($p->isProtected()) {
            $vis = "protected";
            $typ = "any";
        } else {
            $vis = "public";
            $typ = gettype($p->getValue());
        }

        echo "    <field name=\"$p->name\" visibility=\"$vis\"><type name=\"$typ\" /></field>\n";
    }
    echo "   </staticfields>\n";

    echo "   <constants>\n";
    foreach($rc->getConstants() as $name => $c) {

        $typ = gettype($c);

        echo "    <constant name=\"$name\"><type name=\"$typ\" /></constant>\n";
    }
    echo "   </constants>\n";

    echo "   <methods>\n";
    foreach($rc->getMethods() as $m) {
        if ($m->isStatic()) {
            $type = " type=\"static\"";
        } else {
            $type = "";
        }

        if ($m->isPrivate()) {
            $vis = "private";
        } else if ($m->isProtected()) {
            $vis = "protected";
        } else {
            $vis = "public";
        }
        echo "    <method name=\"$m->name\" visibility=\"$vis\"$type>\n";
        echo "     <return><type name=\"any\" /></return>\n";
        echo "     <args>\n";
        foreach($m->getParameters() as $p) {
            if ($p->isDefaultValueAvailable()) {
                $typ = gettype($p->getDefaultValue());
                $type = "<type name=\"".$typ."\" />";

                if ($p->allowsNull() && $typ != "null") {
                    $type .= "<type name=\"null\" />";
                }
            } else {
                $type = "<type name=\"any\" />";
            }

            echo "     <arg opt=\"".intval($p->isOptional())."\" name=\"$p->name\">$type</arg>\n";
        }
        echo "     </args>\n";
        echo "    </method>\n";
    }
    echo "   </methods>\n";

    echo "  </class>\n";
/*
<class name="test">
   <position file="plop.c" line="12" col="9" />

   <fields>
    <field name="test2" visibility="public"><position file="plop.c" line="12" col="10" /><type name="void"></type></field>
   </fields>

   <staticfields>
    <field name="test2" visibility="public"><position file="plop.c" line="12" col="9" /><type name="void"></type></field>
   </staticfields>

   <constants>
    <constant name="test2"><type name="void"></type></constant>
   </constants>

   <methods>
    <method name="plop" visibility="public" type="static">
     <return><type name="void"></type></return>
     <args>
      <arg opt="0"><type name="string"></type></arg>
     </args>
    </method>
   </methods>
</class>*/
}
