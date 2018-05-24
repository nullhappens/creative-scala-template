def numberOfVowels(str: String): Int = str.filter(Seq('a', 'e', 'i', 'o', 'u').contains(_)).length
numberOfVowels("The quick brown fox")

implicit class StringExt(str: String) {
  val vowels = Seq('a', 'e', 'i', 'o', 'u')
  def numberOfVowels: Int = str.filter(vowels contains _).length
}

"The quick brown fox".numberOfVowels

final case class Person(name: String, email: String)

trait HtmlWriter[A] {
  def write(in: A): String
}

implicit object PersonWriter extends HtmlWriter[Person] {
  def write(person: Person) = s"<span>${person.name} &lt;${person.email}&gt;</span>"
}

implicit class HtmlOps[T](data: T){
  def toHtml(implicit writer: HtmlWriter[T]) = writer.write(data)
}

val implicits = Person("diego", "diego@me.com").toHtml
// should be equivalent as
val explicits = HtmlOps[Person](Person("diego", "diego@me.com")).toHtml

assert(implicits == explicits)


// Kool Aid man
object IntOps{
  implicit class IntExts(v: Int){
    def yeah: Unit = (1 to v).toList.foreach(_ => println("oh yeah!"))
    def times(f: Int => Unit): Unit = (1 to v).toList.foreach(f(_))
    def ohyeah: Unit = v.times(_ => println("oh yeah!"))
  }
}

import IntOps._
5.yeah
1.yeah

3.times(_ => println(s"Look at me im mr meeeeeseeeks"))

5.ohyeah


trait Equality[A] {
  def equal(v1: A, v2: A): Boolean
}
object Equality {
  def apply[A](implicit instance: Equality[A]): Equality[A] = instance

  implicit class ToEqual[A](in: A) {
    def ===(other: A)(implicit equal: Equality[A]): Boolean = equal.equal(in, other)
  }
}

implicit val caseInsensitiveEquals: Equality[String] = (a: String, b: String) => a.toLowerCase() == b.toLowerCase()

import Equality._
"test" === "TEST"
