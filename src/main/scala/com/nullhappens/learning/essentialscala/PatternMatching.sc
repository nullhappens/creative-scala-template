final case class Person(firstName: String, lastName: String)

1 + 1 match {
  case 2 => "its two!"
  case _ => "its not two"
}

val X = "Foo"
val Y = "Bar"
val Z = "Baz"

"Bar" match {
  case X => "its Foo"
  case Y => "its Bar"
  case Z => "its Baz"
}

"Bar" match {
  case X | Y => "its foo or bar"
  case Z => "its baz!"
}

123 match {
  case a if a % 2 == 0 => "even"
  case _ => "odd"
}
import scala.util.matching.Regex
val r = new Regex("""(\d+)\.(\d+)\.(\d+)\.(\d+)""")
"192.168.1.1" match {
  case r(a, b, c, d) => List(a, b, c, d)
}

List(1,2,3) match {
  case List(a, b, c) => a + b + c
}

List(1,2,3) match {
  case ::(head, tail) => s"head: $head tail $tail"
  case Nil => "empty"
}

List(1,2,3) match {
  case head :: tail => s"head: $head tail $tail"
  case Nil => "empty"
}

object Email {
  def unapply(str: String): Option[(String, String)] = {
    str.split("@") match {
      case Array(a, b, _*) => Some((a, b))
      case _ => None
    }
  }
}

"dave@underscore.io" match {
  case Email(user, domain) => List(user, domain)
  case _ => List.empty
}

"invalidemail" match {
  case Email(user, domain) => List("user", domain)
  case _ => List.empty
}

object Uppercase {
  def unapply(str: String): Option[String] =
    Some(str.toUpperCase)
}

Person("Dave", "Grohl") match {
  case Person(f, Uppercase(l)) => s"$f $l"
}

object Words {
  def unapplySeq(str: String) =
    Some(str.split(" ").toSeq)
}

"The quick brown fox" match {
  case Words(a, b, c) => s"3 words $a $b $c"
  case Words(a, b, c, d) => s"4 words $a $b $c $d"
}

List(1, 2, 3, 4, 5) match {
  case List(a, b, _*) => a + b
}

List(1, 2, 3, 4, 5) match {
  case List(a, b, rest @ _*) => s"${a + b} plus the $rest"
}

object Positive {
  def unapply(arg: Int): Option[Int] =
    if(arg > 0) Some(arg)
    else None
}

assert(
  "No" ==
    (0 match {
      case Positive(_) => "Yes"
      case _ => "No"
    })
)

assert(
  "Yes" ==
    (42 match {
      case Positive(_) => "Yes"
      case _ => "No"
    })
)

object Titlecase {
  def unapply(arg: String): Option[String] =
    Some(arg
      .split(" ")
      .toList
      .map(_.capitalize)
      .mkString(" ")
    )
}

assert(
  "Sir Lord Doctor David Gurnell" ==
    ("sir lord doctor david gurnell" match {
      case Titlecase(str) => str
    })
)