package experiments

trait SimpleResults {
  type Input
  trait Result[+T] {
    def next: Input
  }
  case class Success[+T](result: T, next: Input) extends Result[T]
  case class Failure(msg: String, next: Input) extends Result[Nothing]
}

object XParser extends SimpleResults {
  type Input = String
  val acceptX: Input ⇒ Result[Char] = { (in: String) ⇒
    if (in.charAt(0) == 'x') Success('x', in.substring(1))
    else Failure("expected an x", in)
  }

  def accept(c: Char): Input ⇒ Result[Char] = { (in: String) ⇒
    if (in.charAt(0) == c) Success(c, in.substring(1))
    else Failure("expected " + c, in)
  }
}

class Parsers {

}
