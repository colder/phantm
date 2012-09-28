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

class TestResult(var errors: Set[String] = Set(), var notices: Set[String] = Set()) {
  def isSuccess = errors.isEmpty
  def containsError(str: String) = errors.exists(_ contains str)
  def containsOnlyError(str: String) = errors.forall(_ contains str)
  def containsNotice(str: String) = notices.exists(_ contains str)
  def containsOnlyNotice(str: String) = notices.forall(_ contains str)
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

  def testFile(settings: Settings, file: File): TestResult = {
    var tr = new TestResult

    class TestReporter(files: List[String]) extends Reporter(files) {
      override def notice(e: String): Boolean = {
        tr.notices += e
        true
      }
      override def error(e: String): Boolean = {
        tr.errors += e
        true
      }
      override def addError(e: Error): Boolean = {
        if (e.tags contains ENotice) {
          tr.notices += e.message
        } else {
          tr.errors += e.message
        }
        true
      }

      override def emitSummary = {

      }
    }


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


  def testFilePath(settings: Settings, path: String): TestResult = {
    testFile(settings, new File(path))
  }
  def testString(settings: Settings, str: String): TestResult = {
    testFile(settings, tmpFileWithContent(str))
  }
}
