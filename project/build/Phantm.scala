import sbt._
import java.io._
import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import Process._

class PhantmProject(info: ProjectInfo) extends DefaultProject(info) with ProguardProject
{
    override def mainClass = Some("phantm.Main")
    override def defaultJarName = "phantm-latest.jar"
    override def compileClasspath = ("lib" / "cup" / "dist" / "java-cup-11a-runtime.jar")

    lazy val `preparefiles` = prepareFilesAction

    override def packageAction = super.packageAction dependsOn(preparefiles)

    def prepareFilesAction = task {
        "test -d src/main/resources/"        #|| "mkdir src/main/resources/" !;
        "test -d src/main/resources/spec/"   #|| "mkdir src/main/resources/spec/" !;
        "test -d src/main/resources/tables/" #|| "mkdir src/main/resources/tables/" !;
        log.info("Creating build.xml")

        val dateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
        val date = new Date()

        val f = new BufferedWriter(new FileWriter("src/main/resources/build.xml"))
        f write "<build><version>"+version+"</version><date>"+dateFormat.format(date)+"</date></build>"
        f close


        log.info("Copying internal_api.xml")
        "cp spec/internal_api.xml src/main/resources/spec/" !;
        log.info("Copying tables")
        "cp src/main/java/phantm/parser/action_table.bin      src/main/resources/tables/" !;
        "cp src/main/java/phantm/parser/production_table.bin  src/main/resources/tables/" !;
        "cp src/main/java/phantm/parser/reduce_table.bin      src/main/resources/tables/" !;
        None
    } describedAs("Prepare files")

    override def minJarName      = "phantm-"+version+".jar"

    override def proguardInJarsArg = Nil
    override def proguardOutJarsArg = Nil

    override lazy val release = proguard

    override def proguardOptions =
        "-ignorewarnings" ::
        "-keep public class phantm.Main { public static void main(java.lang.String[]); }" ::
        "-injars"  :: "lib/cup/dist/java-cup-11a-runtime.jar (!META-INF/**)" ::
        "-injars"  :: System.getenv("SCALA_HOME")+"/lib/scala-library.jar (!META-INF/**)" ::
        "-injars"  :: ""+outputPath+"/"+defaultJarName ::
        "-outjars" :: ""+outputPath+"/"+minJarName::
        Nil
}
