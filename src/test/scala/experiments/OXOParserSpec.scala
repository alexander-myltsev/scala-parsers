package experiments

import org.specs2.mutable.Specification

class OXOParserSpec extends Specification {
  "OXOParser parser" should {
    object OXOParser extends StringParsers {
      def oxo = accept('o') ~ accept('x') ~ accept('o')
      def oxos: Parser[Any] = (oxo ~ accept(' ') ~ oxos | oxo)
    }

    import OXOParser._

    "complete example in Listing 2.5" in {
      val oxoParser = (oxos ~ eoi)
      oxoParser("ooxo") must beAnInstanceOf[Failure]
      oxoParser("oxo") must beAnInstanceOf[Success[_]]
      oxoParser("oxooxo") must beAnInstanceOf[Failure]
      oxoParser("oxo oxo") must beAnInstanceOf[Success[_]]
    }
  }

  "Modified OXOParser parser" should {
    object OXOParser extends StringParsers {
      def oxo: Parser[List[String]] = (accept('o') ~ accept('x') ~ accept('o')) map { case ((a, b), c) ⇒ List("oxo") }
      def oxos: Parser[List[String]] = ((oxo ~ accept(' ') ~ oxos) map { case ((x, c), ls) ⇒ x ::: ls }) | oxo
    }

    import OXOParser._

    "complete Exercise 2.8" in {
      val oxoParser = (oxos ~ eoi) map (_._1)
      println(oxoParser("oxo oxo oxo"))
    }
  }
}
