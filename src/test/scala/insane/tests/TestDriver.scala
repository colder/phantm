package phantm.tests

import java.io._

import scala.xml._

import phantm.util._

import phantm.phases._
import phantm._

import phantm.ast.Trees.Program
import phantm.ast.STToAST

import scala.io.Source

import org.scalatest.FunSuite
import org.scalatest.matchers._

class TestResult(var errors: Set[String] = Set()) {

  def isSuccess = errors.isEmpty
  def containsError(str: String) = errors.exists(_ contains str)
}

class CrashedResult(e: Throwable) extends TestResult(Set(e.getMessage))

trait PhantmTestDriver {

  class IsTestSuccessful extends BeMatcher[TestResult] {
    def apply(left: TestResult) = {
      MatchResult(left.isSuccess, "Errors found: "+left.errors.mkString(", "), "No error found")
    }
  }
  val successful = new IsTestSuccessful
  val failing  = new IsTestSuccessful

  import java.io.{File, BufferedWriter, FileWriter}

  def tmpFileWithContent(content: String): File = {
    val tmpFile = File.createTempFile("phantmtest", ".php");
    tmpFile.deleteOnExit();

    val out = new BufferedWriter(new FileWriter(tmpFile));
    out.write(content);
    out.close();

    tmpFile
  }

  def testStringInput(settings: Settings, str: String): TestResult = {
    var tr = new TestResult

    class TestReporter(files: List[String]) extends Reporter(files) {
      override def notice(e: String): Boolean = {
        tr.errors += e
        true
      }
      override def error(e: String): Boolean = {
        tr.errors += e
        true
      }
      override def addError(e: Error): Boolean = {
        tr.errors += e.message
        true
      }
    }

    val file = tmpFileWithContent(str);

    try {
      val files = List(file.getAbsolutePath)
      val rep = new TestReporter(files)
      Reporter.set(rep)
      Settings.set(settings)
      new PhasesRunner(rep).run(new PhasesContext(files = files))
      tr
    } catch {
      case e: Throwable => new CrashedResult(e)
    }

  }
}
