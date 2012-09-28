package phantm.tests

import scala.io.Source
import phantm.Settings

import org.scalatest.FunSuite
import org.scalatest.matchers._

class DetectionTests extends PhantmTestDriver {

  var settings = Settings(verbosity = 3)

  testAndExpect("tests/target002.php", "Type mismatch")
  testAndExpect("tests/target003.php", "Undefined method 'a::bar'")
  testAndExpect("tests/target004.php", "no side-effect")
  testAndExpect("tests/target005.php", "Type mismatch")
  testAndExpect("tests/target007.php", "no side-effect")
  testAndExpect("tests/target009.php", "Type mismatch")
}
