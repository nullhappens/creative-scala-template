class Cat(val name:String, val colour:String, val food:String)

object ChipShop {
  def willServe(cat: Cat):Boolean =
    if (cat.food.toLowerCase == "chips")
      true
    else
      false
}

class Director(val firstName:String, val lastName:String, val yearOfBirth:Int) {
  def name():String = firstName + " " + lastName

  override def toString: String =
    s"$lastName, $firstName ($yearOfBirth)"
}

object Director {
  def apply(firstName:String, lastName:String, yearOfBirth:Int):Director =
    new Director(firstName, lastName, yearOfBirth)

  def older(a:Director, b:Director):Director =
    if (a.yearOfBirth < b.yearOfBirth) a else b

}

class Film(val name:String, val yearOfRelease:Int, val imdbRating:Double, val director:Director) {

  def directorsAge():Int =
    yearOfRelease - director.yearOfBirth

  def isDirectedBy(x:Director):Boolean =
    director == x

  def copy(name:String = name, yearOfRelease:Int = yearOfRelease, imdbRating:Double = imdbRating, director:Director = director): Film =
    new Film(name, yearOfRelease, imdbRating, director)

  override def toString: String =
    s"$name, $yearOfRelease, $imdbRating / 10 -- $director"

}

object Film {
  def apply(name:String, yearOfRelease:Int, imdbRating:Double, director:Director):Film =
    new Film(name, yearOfRelease, imdbRating, director)

  def highestRating(a:Film, b:Film):Double =
    if (a.imdbRating > b.imdbRating) a.imdbRating else b.imdbRating

  def oldestDirectorAtTheTime(a:Film, b:Film):Director =
    Director.older(a.director,b.director)
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

class Counter(val start:Int){
  def inc(amount:Int = 1):Counter =
    new Counter(start + amount)

  def dec(amount:Int = 1):Counter =
    new Counter(start - amount)

  def inc:Counter = inc()

  def dec:Counter = dec()

  def adjust(adder:Adder) =
    new Counter(adder(start))

  override def toString: String =
    s"$start"
}

new Counter(10).inc.dec.inc.inc(12)

class Adder(amount:Int) {
  def apply(in:Int) = in + amount
}

val add10 = new Adder(10)
val twenty = add10(10)
val tmpCounter = new Counter(10).adjust(add10)


// Companion Objects
class Timestamp(val seconds:Long)

object Timestamp {
  def apply(hours:Int, minutes:Int, seconds:Int):Timestamp =
    new Timestamp(hours*60*60 + minutes*60 + seconds)
}

// Friendly Person Factory

class Person(val firstName:String, val lastName:String)

object Person {
  def apply(completeName:String):Person = {
    val parts = completeName.split(" ")
    new Person(parts(0), parts(1))
  }
}

Person("John Doe").firstName
Person("John Doe").lastName



