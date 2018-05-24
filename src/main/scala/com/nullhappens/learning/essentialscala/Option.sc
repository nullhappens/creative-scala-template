import cats.syntax.OptionIdOps

def addOptions(a: Option[Int], b: Option[Int]): Option[Int] =
  for {
    a1 <- a
    a2 <- b
  } yield a1 + a2

assert(addOptions(Some(2), Some(2)).contains(4))
assert(addOptions(Some(2), None).isEmpty)
assert(addOptions(None, Some(2)).isEmpty)
assert(addOptions(None, None).isEmpty)

def addOptions2(a: Option[Int], b: Option[Int]): Option[Int] =
  a flatMap(a1 => b.map(a1 + _))

assert(addOptions2(Some(2), Some(2)).contains(4))
assert(addOptions2(Some(2), None).isEmpty)
assert(addOptions2(None, Some(2)).isEmpty)
assert(addOptions2(None, None).isEmpty)

def addOptions(a: Option[Int], b: Option[Int], c: Option[Int]): Option[Int] =
  for {
    a1 <- a
    a2 <- b
    a3 <- c
  } yield a1 + a2 + a3

assert(addOptions(Some(2), Some(2), Some(2)).contains(6))
assert(addOptions(Some(2), None, None).isEmpty)
assert(addOptions(None, Some(2), None).isEmpty)
assert(addOptions(None, None, Some(2)).isEmpty)
assert(addOptions(None, None, None).isEmpty)

def addOptions2(a: Option[Int], b: Option[Int], c: Option[Int]): Option[Int] =
  a flatMap(a1 => b flatMap( a2 => c map(a3 => a1 + a2 + a3)))

assert(addOptions2(Some(2), Some(2), Some(2)).contains(6))
assert(addOptions2(Some(2), None, None).isEmpty)
assert(addOptions2(None, Some(2), None).isEmpty)
assert(addOptions2(None, None, Some(2)).isEmpty)
assert(addOptions2(None, None, None).isEmpty)

def divide(num: Int, denom: Int): Option[Int] =
  if (denom == 0) None else Some(num / denom)

assert(divide(2,2).contains(1))
assert(divide(2, 0).isEmpty)

def calculator(operand1: String, operator: String, operand2: String): Unit = {
  def readInt(x: String): Option[Int] = if (x matches "\\d+") Some(x.toInt) else None

  for {
    a <- readInt(operand1)
    b <- readInt(operand2)
  } yield operator match {
    case "+" => println(a + b)
    case "-" => println(a - b)
    case "*" => println(a * b)
    case "/" => if (b != 0) println(a / b) else println("Denominator can't be 0")
    case _ => println("Unsupported operation")
  }
}

calculator("1", "+", "1")
calculator("1", "-", "1")
calculator("4", "/", "2")
calculator("4", "*", "2")
calculator("4", "/", "0")
calculator("4", "^", "0")
