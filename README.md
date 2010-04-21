phantm - PHp ANalzer for Type Mistakes
======

**phantm** is a tool that can be used to spot various kinds of mistakes in your
PHP applications. Among other things, it will perform a static analysis of the
types used in your application, and report any potential mistakes.

Version
-------

This is a early development version. Don't be surprised if many features are still missing.

Requirements
------------
In order to use this tool, you need

* ant
* Java 1.6 or higher
* Scala 2.8.0 or higher

Also, make sure you've the $SCALA_HOME environment variable pointing to your scala distribution.

Installation
------------
Simply run:

    $ make complete

This will, in order, build customized CUP, generate lexer using JFlex, generate parser using CUP, build generated java files, build the scala analyzer.

Usage
-----
To run the analyzer on one of your php script, run

    $ ./phantm <target.php>

The analyzer will then compile your code, and output any notice/warnings it can find about your script. You can also use 

    $ ./phantm --help

to see what options the tool supports:

    Usage:   phantm [..options..] <files ...>
    Options: --help                 This help
             --maindir <maindir>    Specify main directory of the tool
             --colors <mode>        Change the way errors are displayed:
                                    Mode: none   : no colors
                                          termbg : ANSI colors inside the code (default)
                                          term   : ANSI colors below the code
                                          html   : HTML colors below the code
             --symbols              Display symbols
             --showincludes         Display the list of included files
             --noincludes           Disables includes resolutions
             --noapi                Do not load the main API
             --tests                Enable internal consistency checks
             --fixpoint             Display fixpoints
             --debug                Display all kind of debug information
             --quiet                Mute some errors such as uninitialized variables
             --verbose              Display more notices
             --vverbose             Be nitpicking and display even more notices
             --includepath <paths>  Define paths for compile time include resolution (.:a:bb:c:..)
             --importAPI <paths>    Import additional APIs (a.xml:b.xml:...)
             --exportAPI <path>     Use the type analysis to output a likely API
             --progress             Display analysis progress
             --focus                Focus on main files and ignore errors in dependencies
             --only <symbols>       Only do analysis on the specified bodies of code (main:func1:class1::method1:...)
             --exportAPI <path>     Export generated API to <path>
             --lint                 Stop the analysis after the parsingr

VIM Usage
---------
        set makeprg=phantm\ --format\ quickfix\ %
        set errorformat =%W%f:%l:%c\ \ Notice:\ %m
        set errorformat+=%E%f:%l:%c\ \ Error:\ %m
        set errorformat+=%-G%*\\d%m
        set errorformat+=%-G\ %m
        set errorformat+=%-G\ %\\*
