package experiments

trait EagerParsers extends SimpleResults {
  trait Parser[+T] extends (Input ⇒ Result[T]) {
    def apply(in: Input): Result[T]

    // NOTE: | is not `p: => Parser[U]`
    def |[U >: T](p: Parser[U]): Parser[U] = new Parser[U] {
      def apply(in: Input) =
        Parser.this(in) match {
          case Failure(_, _) ⇒ p(in)
          case Success(x, n) ⇒ Success(x, n)
        }
    }

    // NOTE: ~ is not `p: => Parser[U]`
    def ~[U](p: Parser[U]): Parser[Pair[T, U]] = new Parser[Pair[T, U]] {
      def apply(in: Input) = Parser.this(in) match {
        case Success(x, next) ⇒ p(next) match {
          case Success(x2, next2) ⇒ Success((x, x2), next2)
          case Failure(m, n)      ⇒ Failure(m, n)
        }
        case Failure(m, n) ⇒ Failure(m, n)
      }
    }
  }
}

trait EagerStringParsers extends EagerParsers {
  type Input = String

  private val EOI = 0.toChar

  def accept(expected: Char) = new Parser[Char] {
    def apply(in: String) =
      if (in == "") {
        if (expected == EOI)
          Success(expected, "")
        else
          Failure("no more input", in)
      } else if (in.charAt(0) == expected)
        Success(expected, in.substring(1))
      else
        Failure("expected \'" + expected + "\'", in)
  }

  def eoi = accept(EOI)
}

object EagerParserSample extends EagerStringParsers {
  def pars: Parser[Any] = (accept('a') | pars) ~ eoi
}