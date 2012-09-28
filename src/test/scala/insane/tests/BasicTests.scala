package phantm.tests

import scala.io.Source
import phantm.Settings

import org.scalatest.FunSuite
import org.scalatest.matchers._

class BasicTests extends FunSuite with MustMatchers with PhantmTestDriver {

  var settings = Settings(verbosity = 2)

  for (t <- findTests("tests/", "pass-")) {
    test("Analyzing "+t.getName()) {
      val tr = testFilePath(settings, t.getAbsolutePath())
      tr must be (successful)
    }
  }

  for (t <- findTests("tests/", "fail-")) {
    test("Analyzing "+t.getName()) {
      val tr = testFilePath(settings, t.getAbsolutePath())
      tr must not be (successful)
    }
  }
}
