// A traffic light is red, green or yellow

sealed trait TrafficLight {
  def next: TrafficLight =
    this match {
      case Red => Green
      case Green => Yellow
      case Yellow => Red
    }
}

final case object Red extends TrafficLight
final case object Yellow extends TrafficLight
final case object Green extends TrafficLight

// A calculation may succeed (with an Int result)
// or fail (with a String messgge).

sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Error(message: String) extends Calculation

object Calculator {
  def +(calculation: Calculation, value: Int): Calculation =
    calculation match {
      case Success(x) => Success(x + value)
      case Error(message) => Error(message)
    }

  def -(calculation: Calculation, value: Int): Calculation =
    calculation match {
      case Success(x) => Success(x - value)
      case Error(message) => Error(message)
    }

  def /(calculation: Calculation, value: Int): Calculation =
    calculation match {
      case Success(x) => value match {
        case 0 => Error("Division by zero")
        case _ => Success(x / value)
      }
      case Error(msg) => Error(msg)
    }
}

assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(1), 1) == Success(0))
assert(Calculator.+(Error("Badness"), 1) == Error("Badness"))

assert(Calculator./(Success(4), 2) == Success(2))
assert(Calculator./(Success(4), 0) == Error("Division by zero"))
assert(Calculator./(Error("Badness"), 0) == Error("Badness"))

// Bottled water has a size (an Int), a source
// (which is a well, spring, or tap), and a Boolean carbonated

sealed trait Source
final case class Well() extends Source
final case class Spring() extends Source
final case class Tap() extends Source

final case class BottledWater(size: Int, source: Source, carbonated: Boolean)


sealed trait Food
final case object Antelope extends Food
final case object TigerFood extends Food
final case object Licorice extends Food
final case class CatFood(food: String) extends Food

// Polymorphism (overriding methods)
sealed trait Feline {
  def dinner: Food
}
final case class Lion() extends Feline with Feline2 {
  override def dinner: Food = Antelope
}
final case class Tiger() extends Feline with Feline2 {
  override def dinner: Food = TigerFood
}
final case class Panther() extends Feline with Feline2 {
  override def dinner: Food = Licorice
}
final case class Cat(favouriteFood: String) extends Feline with Feline2 {
  override def dinner: Food = CatFood(favouriteFood)
}

// Pattern matching in the base trait (renamed to feline2 for no conflicts)
sealed trait Feline2 {
  def dinner: Food =
    this match {
      case Lion() => Antelope
      case Tiger() => TigerFood
      case Panther() => Licorice
      case Cat(favouriteFood) => CatFood(favouriteFood)
    }
}

// Pattern matching in an external object
object Diner {
  def dinner(feline: Feline): Food =
    feline match {
      case Lion() => Antelope
      case Tiger() => TigerFood
      case Panther() => Licorice
      case Cat(favouriteFood) => CatFood(favouriteFood)
    }
}

