package phpanalysis;

import phpanalysis.parser._;
import phpanalysis.analyzer._;
import phpanalysis.controlflow._;
import phpanalysis.parser.Trees.Program;
import java.io._;
import Math.max;

object Main {
    var files: List[String] = Nil;
    var displaySymbols     = false;
    var displayUsage       = false;
    var verbosity          = 1;
    var resolveIncludes    = true;
    var importAPI          = true;
    var testsActive        = false;
    var displayFixPoint    = false;
    var displayIncludes    = false;
    var displayProgress    = false;
    var focusOnMainFiles   = false;
    var onlyLint           = false;
    var includePaths       = List(".");
    var mainDir            = "./"
    var apis: List[String] = Nil;

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

    def handleArgs(args: List[String]): Unit = args match {
        case "--help" :: xs =>
            displayUsage = true
        case "--maindir" :: x :: xs =>
            mainDir = x
            handleArgs(xs)
        case "--symbols" :: xs =>
            displaySymbols = true
            handleArgs(xs)
        case "--showincludes" :: xs =>
            displayIncludes = true
            handleArgs(xs)
        case "--noincludes" :: xs =>
            resolveIncludes = false
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
        case "--verbose" :: xs =>
            verbosity = max(verbosity, 2)
            handleArgs(xs)
        case "--vverbose" :: xs =>
            verbosity = max(verbosity, 3)
            handleArgs(xs)
        case "--includepath" :: ip :: xs =>
            includePaths = ip.split(":").toList
            handleArgs(xs)
        case "--apis" :: aps :: xs =>
            apis = aps.split(":").toList
            handleArgs(xs)
        case "--progress" :: xs =>
            displayProgress = true
            handleArgs(xs)
        case "--lint" ::  xs =>
            onlyLint = true
            handleArgs(xs)
        case x :: xs =>
            files = files ::: x :: Nil
            handleArgs(xs)
        case Nil =>
    }

    def compile(files: List[String]) = {
        try {
            if (displayProgress) println("1/9 Parsing...")
            val sts = files map { f => val c = new Compiler(f); (c, c compile) }
            if (sts exists { _._2 == None} ) {
                println("Compilation failed.")
            } else {
                if (displayProgress) println("2/9 Simplifying...")
                val asts = sts map { c => new STToAST(c._1, c._2.get) getAST }
                Reporter.errorMilestone
                var ast: Program = asts.reduceLeft {(a,b) => a combine b}
                Reporter.errorMilestone

                if (!onlyLint) {
                    if (importAPI) {
                        if (displayProgress) println("3/9 Importing APIs...")
                        // Load internal classes and functions into the symbol tables
                        new API(mainDir+"spec/internal_api.xml").load

                        for (api <- apis) {
                            new API(api).load
                        }
                    } else {
                        if (displayProgress) println("3/9 Importing APIs (skipped)")
                    }

                    if (resolveIncludes) {
                        ast = ConstantsResolver(ast, false).transform
                        Reporter.errorMilestone

                        if (displayProgress) println("4/9 Resolving includes...")
                        // Run AST transformers
                        ast = IncludeResolver(ast).transform

                        if (displayIncludes) {
                            println("     - Files sucessfully imported:")
                            for (f <- IncludeResolver.includedFiles) {
                                println("       * "+f)
                            }
                        }
                    } else {
                        if (displayProgress) println("4/9 Resolving and expanding (skipped)")
                    }

                    if (displayProgress) println("5/9 Resolving constants...")
                    // Run Constants Resolver, to issue potential errors
                    ast = ConstantsResolver(ast, true).transform
                    Reporter.errorMilestone

                    if (displayProgress) println("6/9 Structural checks...")
                    // Traverse the ast to look for ovious mistakes.
                    new ASTChecks(ast) execute;

                    Reporter.errorMilestone

                    if (displayProgress) println("7/9 Parsing annotations...")
                    // Inject type information from the annotations
                    ast = new Annotations(ast).transform

                    Reporter.errorMilestone

                    if (displayProgress) println("8/9 Symbolic checks...")
                    // Collect symbols and detect obvious types errors
                    CollectSymbols(ast) execute;
                    Reporter.errorMilestone

                    if (displaySymbols) {
                        // Emit summary of all symbols
                        analyzer.Symbols.emitSummary
                    }

                    if (displayProgress) println("9/9 Type flow analysis...")
                    // Build CFGs and analyzes them
                    CFGChecks(ast) execute;
                    Reporter.errorMilestone

                    val n = Reporter.getNoticesCount
                    val tn = Reporter.getTotalNoticesCount

                    if (focusOnMainFiles && n > 0 && tn > n) {
                        println(n+" notice"+(if (n>1) "s" else "")+" occured in main files.")
                        println(tn+" notice"+(if (tn>1) "s" else "")+" occured in total.")
                    } else {
                        println(n+" notice"+(if (n>1) "s" else "")+" occured.")
                    }
                }
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
        println("Usage:   phpanalysis [..options..] <files ...>");
        println("Options: --help                 This help");
        println("         --maindir <maindir>    Specify main directory of the tool");
        println("         --symbols              Display symbols");
        println("         --showincludes         Display the list of included files");
        println("         --noincludes           Disables includes resolutions");
        println("         --noapi                Do not load the main API");
        println("         --tests                Enable internal consistency checks");
        println("         --fixpoint             Display fixpoints");
        println("         --debug                Display all kind of debug information");
        println("         --quiet                Mute some errors such as uninitialized variables");
        println("         --verbose              Display more notices");
        println("         --vverbose             Be nitpicking and display even more notices");
        println("         --includepath <paths>  Define paths for compile time include resolution (.:a:bb:c:..)");
        println("         --apis <paths>         Import additional APIs (a.xml:b.xml:...)");
        println("         --progress             Display analysis progress");
        println("         --focus                Focus on main files and ignore errors in dependencies");
        println("         --lint                 Stop the analysis after the parsing");
    }
}
