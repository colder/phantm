phpanalysis
======

**phpanalysis** is a tool that can be used to spot various kinds of mistakes in your PHP applications

Version
-------

This is a pre-alpha version. Don't be surprised if debug messages are outputted, or if many features are still missing.

Requirements
------------
In order to use this tool, you need

* ant
* Java 1.6 or higher
* Scala 2.7.7 or higher

Installation
------------
Simply run:

    $ make complete

This will, in order, build customized CUP, generate lexer using JFlex, generate parser using CUP, build generated java files, build the scala analyzer.

Usage
-----
To run the analyzer on one of your php script, run

    $ ./phpanalysis <target.php>

The analyser will then compile your code, and output any notice/warnings it can find about your script.
