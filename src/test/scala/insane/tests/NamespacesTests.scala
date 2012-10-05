package phantm.tests

import phantm.Settings

class NamespacesTests extends PhantmTestDriver {

  var settings = Settings(verbosity = 3)

  for (t <- findTests("tests/", "ns-pass-")) {
    testPass("tests/"+t.getName())
  }

  for (t <- findTests("tests/", "ns-fail-")) {
    testFail("tests/"+t.getName())
  }

  testAndExpect("tests/ns001.php", "Undefined class 'Another'")
  testPass("tests/ns002.php") // No errors on undefined classes directly referenced
}
