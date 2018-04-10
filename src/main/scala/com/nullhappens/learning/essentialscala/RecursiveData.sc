import scala.annotation.tailrec

sealed trait IntList {
  def length: Int = {
    @tailrec
    def loop(list: IntList, accumulator: Int = 0): Int =
      list match{
        case End => accumulator
        case Pair(hd, tl) => loop(tl, 1 + accumulator)
      }
    loop(this)
  }

  def product: Int = {
    @tailrec
    def loop(list: IntList, accumulator: Int = 1): Int =
      list match {
        case End => accumulator
        case Pair(hd, tl) => loop(tl, accumulator * hd)
      }

    loop(this)
  }

  def double: IntList =
    this match {
      case End => End
      case Pair(hd, tl) => Pair(hd * 2, tl.double)
    }
}

final case object End extends IntList
final case class Pair(head: Int, tail: IntList) extends IntList

val example = Pair(1, Pair(2, Pair(3, End)))
assert(sum(example) == 6)
assert(sum(example.tail) == 5)
assert(sum(End) == 0)


def sum(list: IntList): Int =
  list match {
    case End => 0
    case Pair(hd, tl) => hd + sum(tl)
  }

@tailrec
def sum(list: IntList, total: Int = 0): Int =
  list match {
    case End => total
    case Pair(hd, tl) => sum(tl, total + hd)
  }

assert(example.length == 3)
assert(example.tail.length == 2)
assert(End.length == 0)

assert(example.product == 6)
assert(example.tail.product == 6)
assert(End.product == 1)

assert(example.double == Pair(2, Pair(4, Pair(6, End))))
assert(example.tail.double == Pair(4, Pair(6, End)))
assert(End.double == End)

//Using
sealed trait Tree {
  def sum: Int
  def double: Tree
}
final case class Node(left: Tree, right: Tree) extends Tree {
  override def sum: Int = left.sum + right.sum
  override def double:Tree = Node(left.double, right.double)
}
final case class Leaf(element: Int) extends Tree {
  override def sum:Int = element
  override def double: Tree = Leaf(element * 2)
}

// polymorphism

object TreeOps {
  def sum(tree: Tree): Int =
    tree match {
      case Leaf(a) => a
      case Node(l, r) => sum(l) + sum(r)
    }

  def double(tree: Tree): Tree =
    tree match {
      case Leaf(a) => Leaf(a * 2)
      case Node(l, r) => Node(double(l), double(r))
    }
}

