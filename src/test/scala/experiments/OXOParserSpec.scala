package experiments

import org.specs2.mutable.Specification

class OXOParserSpec extends Specification {
  import OXOParser._

  "OXOParser parser" should {
    "complete example in Listing 2.5" in {
      val oxoParser = (oxos ~ eoi)
      oxoParser("ooxo") must beAnInstanceOf[Failure]
      oxoParser("oxo") must beAnInstanceOf[Success[_]]
      oxoParser("oxooxo") must beAnInstanceOf[Failure]
      oxoParser("oxo oxo") must beAnInstanceOf[Success[_]]
    }

  }
}
