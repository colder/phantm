package phantm

import java.io._

import phantm.util._

import phantm.phases._

import phantm.ast.Trees.Program
import phantm.ast.STToAST

object Main {
    var settings = Settings()
    var displayUsage = false
    var files = List[String]()

    def main(args: Array[String]): Unit = {
        if (args.length > 0) {
            handleArgs(args.toList)

            if (displayUsage) {
                usage
            } else {
                if (files.length == 0) {
                    println("No file provided.")
                    usage
                } else {
                    val rep = new Reporter(files)
                    Reporter.set(rep)
                    Settings.set(settings)
                    new PhasesRunner(rep).run(new PhasesContext(files = files))
                }
            }
        } else {
            usage
        }
    }

    def handleArgs(args: List[String]): Unit= {
        if (args == Nil) return;
        (args.head.toLowerCase :: args.tail) match {
            case "--help" :: xs =>
                displayUsage = true
            case "--showincludes" :: xs =>
                settings = settings.copy(displayIncludes = true)
                handleArgs(xs)
            case "--noincludes" :: xs =>
                settings = settings.copy(resolveIncludes = false)
                handleArgs(xs)
            case "--format" :: "termbg" :: xs =>
                settings = settings.copy(format = "termbg")
                handleArgs(xs)
            case "--format" :: "term" :: xs =>
                settings = settings.copy(format = "term")
                handleArgs(xs)
            case "--format" :: "html" :: xs =>
                settings = settings.copy(format = "html")
                handleArgs(xs)
            case "--format" :: "quickfix" :: xs =>
                settings = settings.copy(format = "quickfix")
                handleArgs(xs)
            case "--format" :: "none" :: xs =>
                settings = settings.copy(format = "none")
                handleArgs(xs)
            case "--format" :: f :: xs =>
                println("Invalid format "+f)
                displayUsage = true
            case "--only" :: filter :: xs =>
                settings = settings.copy(typeFlowFilter = filter.replace("::", "/").split(":").map(_.replace("/", "::")).toList)
                handleArgs(xs)
            case "--focus" :: xs =>
                settings = settings.copy(focusOnMainFiles = true)
                handleArgs(xs)
            case "--noapi" :: xs =>
                settings = settings.copy(importAPI = false)
                handleArgs(xs)
            case "--tests" :: xs =>
                settings = settings.copy(testsActive = true)
                handleArgs(xs)
            case "--fixpoint" :: xs =>
                settings = settings.copy(displayFixPoint = true)
                handleArgs(xs)
            case "--debug" :: xs =>
                settings = settings.copy(displayFixPoint = true, testsActive = true, displayProgress = true, verbosity = 3)
                handleArgs(xs)
            case "--quiet" :: xs =>
                settings = settings.copy(verbosity = 0)
                handleArgs(xs)
            case "--shy" :: xs =>
                settings = settings.copy(verbosity = -1)
                handleArgs(xs)
            case "--verbose" :: xs =>
                settings = settings.copy(verbosity = 2)
                handleArgs(xs)
            case "--vverbose" :: xs =>
                settings = settings.copy(verbosity = 3)
                handleArgs(xs)
            case "--includepath" :: ip :: xs =>
                settings = settings.copy(includePaths = ip.split(":").toList)
                handleArgs(xs)
            case "--importincludes" :: paths :: xs =>
                //TODO
                IncludeResolver.importIncludes(paths.split(":").toList)
                handleArgs(xs)
            case "--importdump" :: paths :: xs =>
                settings = settings.copy(dumps = paths.split(":").toList)
                handleArgs(xs)
            case "--importapi" :: aps :: xs =>
                settings = settings.copy(apis = aps.split(":").toList)
                handleArgs(xs)
            case "--exportapi" :: path :: xs =>
                settings = settings.copy(exportAPIPath = Some(path))
                handleArgs(xs)
            case "--progress" :: xs =>
                settings = settings.copy(displayProgress = true)
                handleArgs(xs)
            case "--summary" :: xs =>
                settings = settings.copy(summaryOnly = true)
                handleArgs(xs)
            case "--lint" ::  xs =>
                settings = settings.copy(onlyLint = true)
                handleArgs(xs)
            case x :: xs =>
                val f = new File(args.head)
                files = files ::: f.getAbsolutePath :: Nil
                handleArgs(xs)
            case Nil =>
        }
    }

    def usage = {
        println("Usage:   phantm [..options..] <files ...>");
        println("Options: --help                 This help");
        println("         --maindir <maindir>    Specify main directory of the tool");
        println("         --format <mode>        Change the way errors are displayed:");
        println("                                Mode: none     : no colors");
        println("                                      termbg   : ANSI colors inside the code (default)");
        println("                                      term     : ANSI colors below the code");
        println("                                      html     : HTML colors below the code");
        println("                                      quickfix : quickfix error style");
        println("         --showincludes         Display the list of included files");
        println("         --noincludes           Disables includes resolutions");
        println("         --noapi                Do not load the main API");
        println("         --tests                Enable internal consistency checks");
        println("         --fixpoint             Display fixpoints");
        println("         --debug                Display all kind of debug information");
        println("         --quiet                Mute some errors such as uninitialized variables");
        println("         --shy                  Psscht");
        println("         --verbose              Display more notices");
        println("         --vverbose             Be nitpicking and display even more notices");
        println("         --includepath <paths>  Define paths for compile time include resolution (.:a:bb:c:..)");
        println("         --importAPI <paths>    Import additional APIs (a.xml:b.xml:...)");
        println("         --importDUMP <paths>   Import dump files (a.xml:b.xml:...)");
        println("         --exportAPI <path>     Use the type analysis to output a likely API");
        println("         --progress             Display analysis progress");
        println("         --focus                Focus on main files and ignore errors in dependencies");
        println("         --only <symbols>       Only do analysis on the specified bodies of code (main:func1:class1::method1:...)");
        println("         --exportAPI <path>     Export generated API to <path>");
        println("         --lint                 Stop the analysis after the parsing");
    }
}
