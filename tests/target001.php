<?php
class BloException{};
class BlaException extends BloException{};
try {
    throw new BlaException;
} catch(BloException $e) {
    // ...
} catch(BlaException $e) {
    //unreachable
}


