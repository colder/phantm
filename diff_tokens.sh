#!/bin/sh

./helpers.sh $1 > mine.t
php tests/tokenizer.php $1 > php.t

diff -u php.t mine.t
