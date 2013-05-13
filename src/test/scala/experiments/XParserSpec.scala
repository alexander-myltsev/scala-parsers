package experiments

import org.specs2.mutable.Specification

class XParserSpec extends Specification {
  import XParser._

  "XParser parsers" should {
    "complete Excercise 2.1" in {
      acceptX("xyz") must_== (Success('x', "yz"))
      acceptX("xyz") must_!= (Success('x', "yz1"))
    }

    "complete Excercise 2.2" in {
      accept('a')("abc") must_== (Success('a', "bc"))
      accept('a')("abc") must_!= (Success('a', "bc1"))
      accept('a')("abc") must_!= (Success('b', "bc"))
    }
  }
}