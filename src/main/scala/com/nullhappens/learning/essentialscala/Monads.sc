import scala.util.Try

val opt1 = Some(1)
val opt2 = Some(2)
val opt3 = Some(3)

val seq1 = Seq(1)
val seq2 = Seq(2)
val seq3 = Seq(3)

val try1 = Try(1)
val try2 = Try(2)
val try3 = Try(3)

for {
  o1 <- opt1
  o2 <- opt2
  o3 <- opt3
} yield o1 + o2 + o3

for {
  o1 <- seq1
  o2 <- seq2
  o3 <- seq3
} yield o1 + o2 + o3

for {
  o1 <- try1
  o2 <- try2
  o3 <- try3
} yield o1 + o2 + o3

for ((a, b) <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield { a + b }

for {
  x <- Seq(1 , 2, 3, -3) if x < 0
  square = x * x
  y <- Seq(4 , 5 ,6)
} yield square * y

val people = Set(
  "Alice",
  "Bob",
  "Charlie",
  "Derek",
  "Edith",
  "Fred")

val ages = Map(
  "Alice"   -> 20,
  "Bob"     -> 30,
  "Charlie" -> 50,
  "Derek"   -> 40,
  "Edith"   -> 10,
  "Fred"    -> 60)

val favoriteColors = Map(
  "Bob"     -> "green",
  "Derek"   -> "magenta",
  "Fred"    -> "yellow")

val favoriteLolcats = Map(
  "Alice"   -> "Long Cat",
  "Charlie" -> "Ceiling Cat",
  "Edith"   -> "Cloud Cat")

def favoriteColor(pName: String): String = favoriteColors.get(pName) getOrElse "beige"

assert(favoriteColor("Bob") == "green")
assert(favoriteColor("Bobby") == "beige")

def printColors(colors: Map[String, String]) =
  for {
    (name, color) <- colors
  } yield println(color)

printColors(favoriteColors)

def lookup[V](key: String, map: Map[String, V]): Option[V] = map.get(key)

val oldestPerson = ages.foldLeft("" -> 0)( (t1, t2) => if (t1._2 > t2._2) t1 else t2 )
favoriteColors.getOrElse(oldestPerson._1, "Not found")

def union[A](s1: Set[A], s2: Set[A]): Set[A] =
  s1.foldLeft(s2)((r, e) => r + e)

assert(union(Set(1,2,3), Set(1,2,3,4)) == Set(1,2,3,4))

def union[A](m1: Map[A, Int], m2: Map[A, Int]): Map[A, Int] =
  m1.foldLeft(m2) { (acc, el) =>
    val (k, v) = el
    val newVal = acc.get(k) match {
      case Some(v2) => v2 + v
      case None => v
    }
    acc + (k -> newVal)
  }

assert(union(Map("a" -> 1, "b" -> 2), Map("a" -> 1, "b" -> 10, "c" -> 99)) == Map("a" -> 2, "b" -> 12, "c" -> 99))

def union[A, B](m1: Map[A, B], m2: Map[A, B], combine: (B, B) => B): Map[A, B] =
  m1.foldLeft(m2){ (acc, el) =>
    val (k, v) = el
    val newVal = acc.get(k).map(v1 => combine(v, v1)).getOrElse(v)
    acc + (k -> newVal)
  }

val m1 = Map("a" -> 1, "b" -> 2)
val m2 = Map("a" -> 1, "b" -> 10, "c" -> 99)
val m3 = Map("a" -> "a", "b" -> "b")
val m4 = Map("a" -> "b", "b" -> "a", "c" -> "c")
def add1(v1: Int, v2: Int): Int = v1 + v2
def add2(v1: String, v2: String): String = v1 + v2

assert(union(m1, m2, add1) == Map("a" -> 2, "b" -> 12, "c" -> 99))
assert(union(m3, m4, add2) == Map("a" -> "ab", "b" -> "ba", "c" -> "c"))

