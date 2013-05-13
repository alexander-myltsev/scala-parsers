package experiments

import org.specs2.mutable.Specification

class EagerParsersSpec extends Specification {
  import EagerParserSample._

  object LazyParser extends StringParsers {
    def pars: this.Parser[Any] = (accept('a') | pars) ~ eoi
  }

  "Lazy parser" should {
    LazyParser.pars("a") must beAnInstanceOf[Success[_]]
  }

  "Eager parser" should {
    //pars("a") must beAnInstanceOf[Success[_]] // NOTE: This fails with StackOverflowException
  }
}