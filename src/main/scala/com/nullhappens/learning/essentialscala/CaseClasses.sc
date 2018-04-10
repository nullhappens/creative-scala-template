case class Cat(color:String, food:String)


case class Director(val firstName:String, val lastName:String, val yearOfBirth:Int) {
  def name:String = s"$firstName $lastName"
}

object Director {
  def older(a:Director, b:Director):Director =
    if (a.yearOfBirth < b.yearOfBirth) a else b
}

case class Film(val name:String, val yearOfRelease:Int, val imdbRating:Double, val director:Director) {
  def directorsAge():Int =
    yearOfRelease - director.yearOfBirth

  def isDirectedBy(x:Director):Boolean =
    director == x
}

object Film {
  def highestRating(a:Film, b:Film):Double =
    if (a.imdbRating > b.imdbRating) a.imdbRating else b.imdbRating

  def oldestDirectorAtTheTime(a:Film, b:Film):Director =
    Director.older(a.director, b.director)

  def newer(a:Film, b:Film):Film =
    if (a.yearOfRelease > b.yearOfRelease) a else b
}

val eastwood          = new Director("Clint", "Eastwood", 1930)
val mcTiernan         = new Director("John", "McTiernan", 1951)
val nolan             = new Director("Christopher", "Nolan", 1970)
val someBody          = new Director("Just", "Some Body", 1990)

val memento           = new Film("Memento", 2000, 8.5, nolan)
val darkKnight        = new Film("Dark Knight", 2008, 9.0, nolan)
val inception         = new Film("Inception", 2010, 8.8, nolan)

val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7, eastwood)
val outlawJoseyWales  = new Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
val unforgiven        = new Film("Unforgiven", 1992, 8.3, eastwood)
val granTorino        = new Film("Gran Torino", 2008, 8.2, eastwood)
val invictus          = new Film("Invictus", 2009, 7.4, eastwood)

val predator          = new Film("Predator", 1987, 7.9, mcTiernan)
val dieHard           = new Film("Die Hard", 1988, 8.3, mcTiernan)
val huntForRedOctober = new Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

eastwood.yearOfBirth         // should be 1930
dieHard.director.name        // should be "John McTiernan"
invictus.isDirectedBy(nolan) // should be false”

highPlainsDrifter.copy(name = "L'homme des hautes plaines")
// returns Film("L'homme des hautes plaines", 1973, 7.7, /* etc */)

thomasCrownAffair.copy(yearOfRelease = 1968,
  director = new Director("Norman", "Jewison", 1926))
// returns Film("The Thomas Crown Affair", 1926, /* etc */)

inception.copy().copy().copy()
// returns a new copy of `inception`”

case class Adder(amount:Int) {
  def apply(in:Int) = in + amount
}

case class Counter(start:Int = 0) {
  def inc(amount:Int = 1):Counter =
    copy(start + amount)

  def dec(amount:Int = 1):Counter =
    copy(start - amount)

  def inc:Counter = inc()

  def dec:Counter = dec()

  def adjust(adder:Adder) =
    copy(adder(start))
}

Counter(0)
Counter().inc
Counter().inc.dec == Counter().dec.inc


case class Person(firstName:String, lastName:String) {
  def name:String = s"$firstName $lastName"
}

object Person {
  def apply(completeName:String):Person = {
    val parts = completeName.split(" ")
    new Person(parts(0), parts(1))
  }
}

object ChipShop {
  def willServe(cat: Cat):Boolean =
    cat match {
      case Cat(_, "Chips") => true
      case Cat(_, _) => false
    }
}

object Dad {
  def rate(film:Film):Double =
    film.director match {
      case Director("Clint", "Eastwood", _) => 10.0
      case Director("John", "McTiernan", _) => 7.0
      case _ => 3.0
    }
}

Dad.rate(unforgiven)
Dad.rate(huntForRedOctober)
Dad.rate(memento)