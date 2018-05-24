val subjects = List("Noel", "The cat", "The dog")
val verbs = List("wrote", "chased", "slept on")
val objects = List("the book", "the ball", "the bed")

def allSentences(subjects: List[String], verbs: List[String], objects: List[String]): List[String] =
  for {
    s <- subjects
    v <- verbs
    o <- objects
  } yield s"$s $v $o"

allSentences(subjects, verbs, objects)

def conditionalSentences(subjects: List[String]): List[String] = {
  def computeVerb(subject: String): List[String] =
    if (subject == "The cat")
      List("meowed at", "chased", "slept on")
    else if (subject == "The dog")
      List("barked at", "chased", "slept on")
    else
      List("wrote", "chased", "slept on")

  def computeObjects(verb: String): List[String] =
    if (verb == "wrote")
      List("the book", "the letter", "the code")
    else if (verb == "chased")
      List("the ball", "the dog", "the cat")
    else if (verb == "slept on")
      List("the bed", "the mat", "the train")
    else if (verb == "meowed at")
      List("Noel", "The door", "the food cupboard")
    else if (verb == "barked at")
      List("the postman", "the car", "the cat")
    else
      List("the book", "the ball", "the bed")

  for {
    s <- subjects
    v <- computeVerb(s)
    o <- computeObjects(v)
  } yield s"$s $v $o"
}

conditionalSentences(subjects)


final case class Distribution[A](events: List[(A, Double)]) {
  def map[B](f: A => B): Distribution[B] = {
    Distribution(events.map{ case (e, p) => f(e) -> p })
  }

  def flatMap[B](f: A => Distribution[B]): Distribution[B] = {
    Distribution(events.flatMap{ case (e1, p1) =>
        f(e1).events.map { case (e2, p2) =>
          e2 -> p1 * p2
        }
    }).compact.normalize
  }

  def normalize: Distribution[A] = {
    val totalWeight = (events map { case (a, p) => p }).sum
    Distribution(events map { case (a,p) => a -> (p / totalWeight) })
  }

  def compact: Distribution[A] = {
    val distinct = (events map { case (a, p) => a }).distinct
    def prob(a: A): Double =
      (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

    Distribution(distinct map { a => a -> prob(a) })
  }
}

object Distribution {
  def uniform[A](values: List[A]): Distribution[A] = {
    val p = 1.0 / values.length
    Distribution(values.map(v => v -> p))
  }

  def discrete[A](events: List[(A,Double)]): Distribution[A] =
    Distribution(events).compact.normalize
}

sealed trait Coin
final case object Heads extends Coin
final case object Tails extends Coin

val fairCoin: Distribution[Coin] = Distribution.uniform(List(Heads, Tails))
val threeFlips =
  for {
    c1 <- fairCoin
    c2 <- fairCoin
    c3 <- fairCoin
  } yield (c1, c2, c3)


sealed trait Food
final case object Raw extends Food
final case object Cooked extends Food

val food: Distribution[Food] = Distribution.discrete(List(Cooked -> 0.3, Raw -> 0.7))

sealed trait Cat
final case object Asleep extends Cat
final case object Harassing extends Cat

def cat(food: Food): Distribution[Cat] =
  food match {
    case Cooked => Distribution.discrete(List(Harassing -> 0.8, Asleep -> 0.2))
    case Raw => Distribution.discrete(List(Harassing -> 0.4, Asleep -> 0.6))
  }

val foodModel: Distribution[(Food, Cat)] =
  for {
    f <- food
    c <- cat(f)
  } yield (f, c)

// Probability the cat is harassing me
val pHarassing: Double =
  foodModel.events filter {
    case ((_, Harassing), _) => true
    case _ => false
  } map { case (a, p) => p } sum

// Probability the food is cooked given the cat is harassing me
val pCookedGivenHarassing: Option[Double] =
  foodModel.events collectFirst[Double] {
    case ((Cooked, Harassing), p) => p
  } map (_ / pHarassing)


