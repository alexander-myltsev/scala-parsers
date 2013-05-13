package experiments

class ImprovedParsers extends SimpleParsers {
  type Input = String

  private val EOI = 0.toChar

  implicit def accept(expected: Char) = new Parser[Char] {
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
