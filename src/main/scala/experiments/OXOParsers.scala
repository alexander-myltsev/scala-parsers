package experiments

trait SimpleParsers extends SimpleResults {
  abstract class Parser[+T] extends (Input ⇒ Result[T]) {
    def apply(in: Input): Result[T]

    def flatMap[U](f: T ⇒ Parser[U]): Parser[U] = new Parser[U] {
      def apply(in: Input) = Parser.this(in) flatMapWithNext (f)
    }

    def map[U](f: T ⇒ U): Parser[U] = new Parser[U] {
      def apply(in: Input) = Parser.this(in) map (f)
    }

    def |[U >: T](p: ⇒ Parser[U]): Parser[U] = new Parser[U] {
      def apply(in: Input) = Parser.this(in) append p(in)
    }

    def ~[U](p: ⇒ Parser[U]): Parser[(T, U)] = for (a ← this; b ← p) yield (a, b)
  }
}

trait StringParsers extends SimpleParsers {
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