package phantm.tests

import scala.io.Source
import phantm.Settings

import org.scalatest.FunSuite
import org.scalatest.matchers._

class BasicTests extends FunSuite with MustMatchers with PhantmTestDriver {

  var settings = Settings(verbosity = 2)

  for (t <- findTests("tests/", "pass-")) {
    testPass("tests/"+t.getName())
  }

  for (t <- findTests("tests/", "fail-")) {
    testFail("tests/"+t.getName())
  }

  testPass("tests/inc-001.php")
}
