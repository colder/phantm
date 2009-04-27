package phpanalysis.helpers;

trait Helper {
    def generate(input: String, output: String): Unit = {
        val outputStream = new java.io.FileOutputStream(output);
        val printStream  = new java.io.PrintStream(outputStream);
        generate(input, printStream);
    }
    def generate(input: String, printStream: java.io.PrintStream): Unit;
}
