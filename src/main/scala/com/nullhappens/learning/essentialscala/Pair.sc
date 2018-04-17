final case class Pair[A, B](one: A, two: B)

val pair = Pair[String, Int]("hi", 2)
pair.one
pair.two
