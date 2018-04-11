import scala.annotation.tailrec

sealed trait LinkedList[A] {
  // for the purposes of testing recursion
  def length: Int = {
    @tailrec
    def loop(list: LinkedList[A], accumulator: Int = 0): Int =
      list match{
        case End() => accumulator
        case Pair(hd, tl) => loop(tl, 1 + accumulator)
      }
    loop(this)
  }

  def contains(element: A): Boolean = {
    @tailrec
    def loop(list: LinkedList[A], accumulator: Boolean = false): Boolean =
      list match {
        case End() => accumulator
        case Pair(hd, tl) =>
          if (hd == element)
            true
          else
            loop(tl)
      }
    loop(this)
  }

  def apply(index: Int): Result[A] = {
    @tailrec
    def loop(list: LinkedList[A], counter: Int = 0): Result[A] =
      list match {
        case End() => Failure("Index out of bounds")
        case Pair(hd, tl) =>
          if (counter == index)
            Success(hd)
          else
            loop(tl, counter + 1)
      }
    loop(this)
  }
}

final case class End[A]() extends LinkedList[A]
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

final case class Box[A](value: A)
Box(2)
Box("hi")
Box(2.0)

def generic[A](in: A): A = in
generic[String]("Hello")
generic(2)

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

val example = Pair(1, Pair(2, Pair(3, End())))
assert(example.length == 3)
assert(example.tail.length == 2)
assert(End().length == 0)

val example2 = Pair(1, Pair(2, Pair(3, End())))
assert(example2.contains(3) == true)
assert(example2.contains(4) == false)
assert(End().contains(0) == false)
// This should not compile
// example2.contains("not an Int")

val example3 = Pair(1, Pair(2, Pair(3, End())))
assert(example3(0) == Success(1))
assert(example3(1) == Success(2))
assert(example3(2) == Success(3))
assert(example3(3) == Failure("Index out of bounds"))