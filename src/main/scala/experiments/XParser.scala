package experiments

trait SimpleResults {
  type Input

  trait Result[+T] {
    def next: Input
    def map[U](f: T ⇒ U): Result[U]
    def flatMapWithNext[U](f: T ⇒ Input ⇒ Result[U]): Result[U]
    def append[U >: T](alt: ⇒ Result[U]): Result[U]
  }

  case class Success[+T](result: T, next: Input) extends Result[T] {
    def map[U](f: T ⇒ U) = Success(f(result), next)
    def flatMapWithNext[U](f: T ⇒ Input ⇒ Result[U]) = f(result)(next)
    def append[U >: T](alt: ⇒ Result[U]) = this
  }

  case class Failure(msg: String, next: Input) extends Result[Nothing] {
    def map[U](f: Nothing ⇒ U) = this
    def flatMapWithNext[U](f: Nothing ⇒ Input ⇒ Result[U]) = this
    def append[U](alt: ⇒ Result[U]) = alt
  }
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