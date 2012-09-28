package phantm.tests

import scala.io.Source
import phantm.Settings

import org.scalatest.FunSuite
import org.scalatest.matchers._

class BasicPassTests extends FunSuite with MustMatchers with PhantmTestDriver {

  var settings = Settings()
  var settingsOnlyErrors = Settings(verbosity = 3)


  test("Parsing 001") {

    val tr = testString(settings, """
plop
plop
    """)


    tr must be (successful)

  }

  for (i <- 1 to 16) {
    val path = "tests/%03d.php".format(i)
    test("Parsing "+path) {
      val tr = testFilePath(settingsOnlyErrors, path)
      tr must be (successful)
    }
  }
}
