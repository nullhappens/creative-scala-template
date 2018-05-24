final case class Person(name: String, email: String)
val person = Person("John Smith", "jsmith@aol.com")

trait HtmlWriter[A] {
  def write(in: A): String
}
object HtmlWriter {
  def apply[A](implicit writer: HtmlWriter[A]): HtmlWriter[A] = writer
}

implicit object PersonWriter extends HtmlWriter[Person] {
  def write(person: Person) = s"<span>${person.name} &lt;${person.email}&gt;</span>"
}

HtmlWriter[Person].write(person)

object HtmlUtil {
  def htmlify[A](data: A)(implicit writer: HtmlWriter[A]): String =
    writer.write(data)
}

HtmlUtil.htmlify(person)

implicit object ApproximationWriter extends HtmlWriter[Int] {
  override def write(in: Int): String =
    s"It's definitely less than ${ ((in / 10) + 1) * 10}"
}

HtmlUtil.htmlify(2)

// Person equality
trait Equal[A] {
  def equal(v1: A, v2: A): Boolean
}

object EmailEqual extends Equal[Person] {
  def equal(v1: Person, v2: Person): Boolean =
    v1.email == v2.email
}

implicit object NameEmailEqual extends Equal[Person] {
  def equal(v1: Person, v2: Person): Boolean =
    v1.email == v2.email && v1.name == v2.name
}
// Two different styles
object Eq {
  def apply[A](a: A, b: A)(implicit op: Equal[A]): Boolean = op.equal(a, b)
}

assert(Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com")))

// vs

object Equal {
  def apply[A](implicit eq: Equal[A]): Equal[A] = eq
}

assert(Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com")))

