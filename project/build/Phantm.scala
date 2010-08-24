import sbt._
import Process._

class PhantmProject(info: ProjectInfo) extends DefaultProject(info)
{
    override def mainClass = Some("phantm.Main")
    override def defaultJarName = "phantm-"+version+".jar"

    lazy val `preparefiles` = prepareFilesAction

    override def packageAction = super.packageAction dependsOn(preparefiles)

    def prepareFilesAction = task {
        "test -d src/main/resources/"        #|| "mkdir src/main/resources/" !;
        "test -d src/main/resources/spec/"   #|| "mkdir src/main/resources/spec/" !;
        "test -d src/main/resources/tables/" #|| "mkdir src/main/resources/tables/" !;
        log.info("Copying build.xml")
        "cp spec/build.xml src/main/resources/spec/" !;
        log.info("Copying internal_api.xml")
        "cp spec/internal_api.xml src/main/resources/spec/" !;
        log.info("Copying tables")
        "cp src/main/java/phantm/parser/action_table.bin      src/main/resources/tables/" !;
        "cp src/main/java/phantm/parser/production_table.bin  src/main/resources/tables/" !;
        "cp src/main/java/phantm/parser/reduce_table.bin      src/main/resources/tables/" !;
        None } describedAs("Prepare files")

}
