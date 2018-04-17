sealed trait Maybe[A] {
  def fold[B](full: A => B, empty: B) =
    this match {
      case Empty() => empty
      case Full(x) => full(x)
    }
}

final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

val perhaps: Maybe[Int] = Empty[Int]()
val perhaps2: Maybe[Int] = Full(1)

perhaps2.fold[Int]((_:Int) * 2, 0)
