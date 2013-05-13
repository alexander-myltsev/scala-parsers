package experiments

import org.specs2.mutable.Specification

class ImprovedParserSpec extends Specification {
  object OXOParserImproved extends ImprovedParsers {
    def oxo = 'o' ~ 'x' ~ 'o'
    def oxos: Parser[Any] = (oxo ~ ' ' ~ oxos | oxo)
  }

  import OXOParserImproved._

  "OxoParser-Improved" should {
    "equal to OxoParser" in {
      val oxoParser = (oxos ~ eoi)
      oxoParser("ooxo") must beAnInstanceOf[Failure]
      oxoParser("oxo") must beAnInstanceOf[Success[_]]
      oxoParser("oxooxo") must beAnInstanceOf[Failure]
      oxoParser("oxo oxo") must beAnInstanceOf[Success[_]]
    }
  }
}