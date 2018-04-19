sealed trait Maybe[+A] {
  def fold[B](full: A => B, empty: B) =
    this match {
      case Empty => empty
      case Full(x) => full(x)
    }

  def flatMap[B](fn: A => Maybe[B]): Maybe[B] =
    this match {
      case Empty => Empty
      case Full(value) => fn(value)
    }

  def map[B](fn: A => B): Maybe[B] =
    this match {
      case Empty => Empty
      case Full(value) => Full(fn(value))
    }
}

final case class Full[A](value: A) extends Maybe[A]
final case object Empty extends Maybe[Nothing]

val perhaps: Maybe[Int] = Empty
val perhaps2: Maybe[Int] = Full(1)

perhaps2.fold[Int]((_:Int) * 2, 0)
