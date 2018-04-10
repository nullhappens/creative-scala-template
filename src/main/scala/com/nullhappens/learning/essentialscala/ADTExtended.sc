sealed trait Expression {
  def eval: Calculation = {
    this match {
      case Addition(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(l1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r1) => Success(l1 + r1)
            }
        }
      case Substraction(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(l1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r1) => Success(l1 + r1)
            }
        }
      case Division(l, r) =>
        l.eval match {
          case Failure(reason) => Failure(reason)
          case Success(l1) =>
            r.eval match {
              case Failure(reason) => Failure(reason)
              case Success(r1) =>
                if (r1 == 0)
                  Failure("Division by zero!")
                else
                  Success(l1 / r1)
            }
        }
      case SquareRoot(v) =>
        v.eval match {
          case Failure(reason) => Failure(reason)
          case Success(v) =>
            if (v < 0)
              Failure("Complex number!")
            else
              Success(Math.sqrt(v))
        }
      case Number(a) => Success(a)
    }
  }

}
final case class Addition(lhs: Expression, rhs: Expression) extends Expression
final case class Substraction(lhs: Expression, rhs: Expression) extends Expression
final case class Division(lhs: Expression, rhs: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
final case class Number(value: Double) extends Expression

sealed trait Calculation
final case class Success(result: Double) extends Calculation
final case class Failure(message: String) extends Calculation

assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Complex number!"))
assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
assert(Division(Number(4), Number(0)).eval == Failure("Division by zero!"))




