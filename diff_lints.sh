#!/bin/sh

(./helpers.sh -t lint $1 2>&1) > mine.t
(php -l $1 2>&1) > php.t

diff -u php.t mine.t
