package phantm

import java.io._

import phantm.util._
import phantm.analyzer._
import phantm.controlflow._

import phantm.phases._

import phantm.ast.Trees.Program
import phantm.ast.STToAST

object Main {
    var files: List[String] = Nil;
    var displayUsage       = false;
    var verbosity          = 1;
    var format             = "termbg";
    var resolveIncludes    = true;
    var importAPI          = true;
    var testsActive        = false;
    var displayFixPoint    = false;
    var displayIncludes    = false;
    var displayProgress    = false;
    var focusOnMainFiles   = false;
    var onlyLint           = false;
    var typeFlowFilter     = List[String]();
    var includePaths       = List(".");
    var mainDir            = "./"
    var apis               = List[String]();
    var dumps              = List[String]();
    var dumpedData         = List[Unserializer]();
    var exportAPIPath: Option[String] = None;

    def main(args: Array[String]): Unit = {
        if (args.length > 0) {
            handleArgs(args.toList);
            if (displayUsage) {
                usage
            } else {
                if (files.length == 0) {
                    println("No file provided.")
                    usage
                } else {
                    compile(files)
                }
            }
        } else {
            usage
        }
    }

    def handleArgs(args: List[String]): Unit = {
        if (args == Nil) return;
        (args.head.toLowerCase :: args.tail) match {
            case "--help" :: xs =>
                displayUsage = true
            case "--maindir" :: x :: xs =>
                mainDir = x
                handleArgs(xs)
            case "--showincludes" :: xs =>
                displayIncludes = true
                handleArgs(xs)
            case "--noincludes" :: xs =>
                resolveIncludes = false
                handleArgs(xs)
            case "--format" :: "termbg" :: xs =>
                format = "termbg";
                handleArgs(xs)
            case "--format" :: "term" :: xs =>
                format = "term";
                handleArgs(xs)
            case "--format" :: "html" :: xs =>
                format = "html";
                handleArgs(xs)
            case "--format" :: "quickfix" :: xs =>
                format = "quickfix";
                handleArgs(xs)
            case "--format" :: "none" :: xs =>
                format = "none";
                handleArgs(xs)
            case "--format" :: f :: xs =>
                println("Invalid format "+f)
            case "--only" :: filter :: xs =>
                typeFlowFilter = filter.replace("::", "/").split(":").map(_.replace("/", "::")).toList
                handleArgs(xs)
            case "--focus" :: xs =>
                focusOnMainFiles = true
                handleArgs(xs)
            case "--noapi" :: xs =>
                importAPI = false
                handleArgs(xs)
            case "--tests" :: xs =>
                testsActive = true
                handleArgs(xs)
            case "--fixpoint" :: xs =>
                displayFixPoint = true
                handleArgs(xs)
            case "--debug" :: xs =>
                displayFixPoint = true
                testsActive     = true
                displayProgress = true
                verbosity       = 3
                handleArgs(xs)
            case "--quiet" :: xs =>
                verbosity = 0
                handleArgs(xs)
            case "--shy" :: xs =>
                verbosity = -1
                handleArgs(xs)
            case "--verbose" :: xs =>
                verbosity = verbosity.max(2)
                handleArgs(xs)
            case "--vverbose" :: xs =>
                verbosity = verbosity.max(3)
                handleArgs(xs)
            case "--includepath" :: ip :: xs =>
                includePaths = ip.split(":").toList
                handleArgs(xs)
            case "--importincludes" :: paths :: xs =>
                IncludeResolver.importIncludes(paths.split(":").toList)
                handleArgs(xs)
            case "--importdump" :: paths :: xs =>
                dumps = paths.split(":").toList
                handleArgs(xs)
            case "--importapi" :: aps :: xs =>
                apis = aps.split(":").toList
                handleArgs(xs)
            case "--exportapi" :: path :: xs =>
                exportAPIPath = Some(path)
                handleArgs(xs)
            case "--progress" :: xs =>
                displayProgress = true
                handleArgs(xs)
            case "--lint" ::  xs =>
                onlyLint = true
                handleArgs(xs)
            case x :: xs =>
                files = files ::: args.head :: Nil
                handleArgs(xs)
            case Nil =>
        }
    }

    def compile(files: List[String]) = {
        try {
            var ctx = new PhasesContext().setFiles(files)

            var oph: Option[Phase] = Some(CompilationPhase)

            var i = 1;
            while(oph != None) {
                val ph = oph.get
                try {
                    if (displayProgress) {
                        println(i+": "+ph.name+"...")
                    }
                    ctx = ph.run(ctx)
                    Reporter.errorMilestone
                    oph = ph.next
                    i += 1
                } catch {
                    case e: PhaseException =>
                        Reporter.error("Processing failed at phase "+i+" ("+e.ph.name+"): "+e.error)
                        Reporter.errorMilestone
                }
            }
            val n = Reporter.getNoticesCount
            val tn = Reporter.getTotalNoticesCount

            if (focusOnMainFiles && n > 0 && tn > n) {
                println(n+" notice"+(if (n>1) "s" else "")+" occured in main files.")
                println(tn+" notice"+(if (tn>1) "s" else "")+" occured in total.")
            } else {
                println(n+" notice"+(if (n>1) "s" else "")+" occured.")
            }

        } catch {
            case Reporter.ErrorException(en, nn, etn, ntn) =>
                if (focusOnMainFiles) {
                    println(nn+" notice"+(if (nn>1) "s" else "")+" and "+en+" error"+(if (en>1) "s" else "")+" occured in main files, abort.")
                    println(ntn+" notice"+(if (ntn>1) "s" else "")+" and "+etn+" error"+(if (etn>1) "s" else "")+" occured in total.")
                } else {
                    println(nn+" notice"+(if (nn>1) "s" else "")+" and "+en+" error"+(if (en>1) "s" else "")+" occured, abort.")

                }
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
