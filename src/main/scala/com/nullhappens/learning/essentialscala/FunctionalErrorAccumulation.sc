import java.time.LocalDate

import scala.util.{ Either, Left, Right }

sealed abstract class RoastLevel(val value: Int)
object RoastLevel {
  case object VeryLight extends RoastLevel(1)
  case object Light     extends RoastLevel(2)
  case object Medium    extends RoastLevel(3)
  case object Dark      extends RoastLevel(4)
  case object Burnt     extends RoastLevel(5)
}

trait Roast {
  def level: RoastLevel
  def date: LocalDate
  def isEven: Boolean
}
case class UnevaluatedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast
case class ApprovedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast

case class RoastProblem(reason: String)

object RoastEvaluation {
  def evaluateRoastLevel(roastLevel: RoastLevel): Option[RoastProblem] = {
    if (roastLevel.value > 2)
      None
    else
      Some(RoastProblem(s"roast too light, at a ${roastLevel.value}"))
  }

  def evaluateFreshness(roastDate: LocalDate): Option[RoastProblem] = {
    if (roastDate.isAfter(LocalDate.now.minusDays(3)))
      None
    else
      Some(RoastProblem(s"not fresh, roast date ${roastDate} is more than 3 days old"))
  }

  def evaluateEvenness(roastIsEven: Boolean): Option[RoastProblem] = {
    if (roastIsEven)
      None
    else
      Some(RoastProblem("roast is not evenly distributed"))
  }

  def evaluateRoast(roast: Roast): Either[List[RoastProblem], ApprovedRoast] = {
    val problems = List(
      evaluateRoastLevel(roast.level),
      evaluateFreshness(roast.date),
      evaluateEvenness(roast.isEven)).flatten

    if (problems.isEmpty)
      Right(ApprovedRoast(roast.level, roast.date, roast.isEven))
    else
      Left(problems)
  }
}