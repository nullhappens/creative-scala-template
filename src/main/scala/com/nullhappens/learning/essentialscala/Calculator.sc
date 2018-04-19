sealed trait Sum[+A, +B] {
  def fold[C](error: A => C , success: B => C): C =
    this match {
      case Failure(x) => error(x)
      case Success(x) => success(x)
    }

  def map[C](fn: B => C): Sum[A, C] =
    this match {
      case Failure(x) => Failure(x)
      case Success(y) => Success(fn(y))
    }

  def flatMap[AA >: A, C](fn: B => Sum[AA,C]): Sum[AA, C] =
    this match {
      case Failure(x) => Failure(x)
      case Success(y) => fn(y)
    }
}
final case class Failure[A, B](value: A) extends Sum[A, Nothing]
final case class Success[A, B](value: B) extends Sum[Nothing, B]

sealed trait Expression {
  def eval: Sum[String, Double] =
    this match {
      case Addition(l, r) => lift2(l, r, (left, right) => Success(left + right))
      case Subtraction(l, r) => lift2(l, r, (left, right) => Success(left - right))
      case Division(l, r) => lift2(l, r, (left, right) =>
        if (right == 0) {
          Failure("Division by zero")
        } else {
          Success(left / right)
        }
      )
      case SquareRoot(v) => v.eval flatMap{ value =>
        if (value < 0) {
          Failure("Square root of negative number")
        } else {
          Success(Math.sqrt(value))
        }
      }
      case Number(v) => Success(v)
    }

  def lift2(lhs: Expression, rhs: Expression, f: (Double, Double) => Sum[String, Double]) =
    lhs.eval flatMap { left =>
      rhs.eval flatMap { right =>
        f(left, right)
      }
    }
}
final case class Addition(lhs: Expression, rhs: Expression) extends Expression
final case class Subtraction(lhs: Expression, rhs: Expression) extends Expression
final case class Division(lhs: Expression, rhs: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
final case class Number(value: Int) extends Expression


assert(Addition(Number(1), Number(2)).eval == Success(3))
assert(SquareRoot(Number(-1)).eval == Failure("Square root of negative number"))
assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
assert(Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval == Success(2.0))