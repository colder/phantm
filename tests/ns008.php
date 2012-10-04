<?php
declare(encoding='UTF-8');
namespace MyProject {

    const CONNECT_OK = 1;
    class Connection { public static function start() { } /* ... */ }
        function connect() { /* ... */  }
}

namespace { // global code
    session_start();
    $a = MyProject\connect();
    echo MyProject\Connection::start();
}
?>
