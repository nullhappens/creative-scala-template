import scala.math._

val minOrdering = Ordering.fromLessThan[Int](_ < _)
val maxOrdering = Ordering.fromLessThan[Int](_ > _)

List(4,5,6,10,-1).sorted(minOrdering)
List(4,5,6,10,-1).sorted(maxOrdering)
List(4,5,6,10,-1).sorted

implicit val absOrdering: Ordering[Int] = Ordering.fromLessThan[Int](abs(_) < abs(_))

assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))
assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))

final case class Rational(numerator: Int, denominator: Int) {
  def < (that: Rational): Boolean =
    (this.numerator.toDouble / this.denominator.toDouble) < (that.numerator.toDouble / that.denominator.toDouble)
}
object Rational {
  implicit val ordering: Ordering[Rational] = Ordering.fromLessThan(_ < _)
}

assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted == List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))


final case class Order(units: Int, unitPrice: Double) {
  val totalPrice: Double = unitPrice * units
}
object OrderingByTotalPrice {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan(_.totalPrice < _.totalPrice)
}
object OrderingByTotalCount {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan(_.units < _.units)
}
object OrderingByUnitPrice {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
}


sealed trait HtmlWriteable {
  def toHtml: String
}

final case class Person(name: String, email: String) extends HtmlWriteable {
  override def toHtml: String = s"<span>$name &lt;$email&gt;</span>"
}

Person("John Smith", "jsmith@aol.com").toHtml

sealed trait HtmlWriter[A] {
  def write(in: A): String
}
object PersonWriter extends HtmlWriter[Person] {
  def write(in: Person): String = s"<span>${in.name} &lt;${in.email}&gt;</span>"
}

PersonWriter.write(Person("Test Testerson", "test@testerson.com"))

import java.util.Date

object DateWriter extends HtmlWriter[Date] {
  def write(in: Date) = s"<span>${in.toString}</span>"
}
DateWriter.write(new Date())

object ObfuscatedPersonWriter extends HtmlWriter[Person] {
  def write(person: Person) =
    s"<span>${person.name} (${person.email.replaceAll("@", " at ")})</span>"
}

ObfuscatedPersonWriter.write(Person("John", "john@example.com"))

trait Equal[A] {
  def equal(a: A, b: A): Boolean
}

object EqualityByEmail extends Equal[Person] {
  override def equal(a: Person, b: Person): Boolean = a.email == b.email
}

assert(EqualityByEmail.equal(Person("", "test@test.com"), Person("", "test@test.com")))
assert(!EqualityByEmail.equal(Person("", "wompwomp@test.com"), Person("", "test@test.com")))

object EqualityByNameEmail extends Equal[Person] {
  override def equal(a: Person, b: Person): Boolean = EqualityByEmail.equal(a, b) && a.name == b.name
}

assert(EqualityByNameEmail.equal(Person("Test", "test@test.com"), Person("Test", "test@test.com")))
assert(!EqualityByNameEmail.equal(Person("Test", "wompwomp@test.com"), Person("Testerson", "test@test.com")))

