package phantm.tests

import scala.io.Source
import phantm.Settings

import org.scalatest.FunSuite
import org.scalatest.matchers._

class Parsing extends FunSuite with MustMatchers with PhantmTestDriver {


  test("Parsing 001") {

    var settings = Settings()
    val tr = testStringInput(settings, """
plop
plop
    """)


    tr must be (successful)

  }
}
