phantm - PHp ANalzer for Type Mismatches
======

**phantm** is a tool that can be used to spot various kinds of mistakes in your
PHP applications. Among other things, it will perform a static analysis of the
types used in your application, and report any potential mistakes.

Version
-------

This is a early development version. Don't be surprised if many features are still missing.

Requirements
------------
In order to run this tool, simply use a self-contained release that you'll find on the download pages.
You only need Java 1.6 or higher to run a release of phantm.


In order to compile this tool, you need

* ant
* sbt
* Java 1.6 or higher
* Scala 2.9.1 or higher

Also, make sure you've the $SCALA_HOME environment variable pointing to your scala distribution.

Installation
------------
The first time you compile phantm, run:

    $ make bootstrap

This will, in order, build customized CUP, generate lexer using JFlex, generate parser using CUP, and build the generated java files.

To build the core component of phantm, the analyzer, you need sbt (specifically, you need a version <u>older</u> than sbt 0.10. You should be able to download sbt 0.7.7 from <a href="http://code.google.com/p/simple-build-tool/">here</a>).

Invoke sbt as follows:

    $ sbt package

This will download the required version of Scala and other dependencies using Ivy, and produce a jar file containing phantm, as well as a script called "phantm". You can use this script to analyze your PHP programs.

You can also package everything into a single jar by invoking

    $ sbt proguard

You can then also run phantm with

    $ java -jar phantm-1.?.?.jar

Usage
-----
To run the analyzer on one of your php script, run

    $ ./phantm <target.php>

The analyzer will then compile your code, and output any notice/warnings it can find about your script. You can also use 

    $ ./phantm --help

to see what options the tool supports:

    Usage:   phantm [..options..] <files ...>
    Options:

      - General settings:
             --maindir <maindir>    Specify main directory of the tool
             --includepath <paths>  Define paths for compile time include resolution (.:a:bb:c:..)
             --only <symbols>       Only perform analysis on the specified
                                            symbols (main:func1:class1::method1:...)

      - Error control:
             --format <mode>        Change the way errors are displayed:
                                    Mode: none     : no colors
                                          termbg   : ANSI colors inside the code (default)
                                          term     : ANSI colors below the code
                                          html     : HTML colors below the code
                                          quickfix : quickfix error style
             --quiet                Mute some errors such as uninitialized variables
             --shy                  Psscht
             --verbose              Display more notices
             --compactErrors yes|no Group errors per line. Useful when inlining
             --vverbose             Be nitpicking and display even more notices

      - Additional features/infos:
             --noapi                Do not load the main API
             --noincludes           Disables includes resolutions
             --fixpoint             Display fixpoints
             --showincludes         Display the list of included files
             --importAPI <paths>    Import additional APIs (a.xml:b.xml:...)
             --importState <paths>  Import state files (i.e. last.dump)
             --exportAPI <path>     Use the type analysis to output a likely API
             --exportCG <path>      Export the call graph in dot format to <path>
             --exportMG <path>      Export the method inheritence graph in dot format to <path>
             --progress             Display analysis progress
             --inline <mode>        Perform function/method inlining, default is 'manual'
                                    Mode: none     : no inlining
                                          manual   : Inline methods specified in the code
                                          leaves   : Inline leaves in the callgraph
                                          full     : Inline acyclic functions
             --anyError yes|[no]    Assume correct inputs

      - Misc.:
             --tests                Enable internal consistency checks
             --lint                 Stop the analysis after the parsing
             --debug                Display all kind of debug information
             --help                 This help
             --                     Separate options and files, allowing files starting with '-'.

VIM Usage
---------
        set makeprg=phantm\ --format\ quickfix\ %
        set errorformat =%W%f:%l:%c\ \ Notice:\ %m
        set errorformat+=%E%f:%l:%c\ \ Error:\ %m
        set errorformat+=%-G%*\\d%m
        set errorformat+=%-G\ %m
        set errorformat+=%-G\ %\\*
