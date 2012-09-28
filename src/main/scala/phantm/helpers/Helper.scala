package phantm.helpers;
import phantm.phases.PhasesContext

trait Helper {
    def generate(input: String, output: String, ctx: PhasesContext): Unit = {
        val outputStream = new java.io.FileOutputStream(output);
        val printStream  = new java.io.PrintStream(outputStream);
        generate(input, printStream, ctx);
    }
    def generate(input: String, printStream: java.io.PrintStream, ctx: PhasesContext): Unit;
}
