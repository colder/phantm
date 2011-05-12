#!/bin/sh
DIR=`dirname $0`
scala -classpath $DIR/target/scala_2.9.0.RC1/phantm-latest.jar:$DIR/lib/cup/dist/java-cup-11a-runtime.jar phantm.helpers.Main $*
