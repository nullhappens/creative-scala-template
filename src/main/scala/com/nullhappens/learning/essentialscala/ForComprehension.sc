final case class Film(name: String, yearOfRelease: Int, imdbRating: Double)

final case class Director(firstName: String, lastName: String, yearOfBirth: Int, films: Seq[Film])

val memento = Film("Memento", 2000, 8.5)
val darkKnight = Film("Dark Knight", 2008, 9.0)
val inception = Film("Inception", 2010, 8.8)
val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)
val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9)
val unforgiven = Film("Unforgiven", 1992, 8.3)
val granTorino = Film("Gran Torino", 2008, 8.2)
val invictus = Film("Invictus", 2009, 7.4)

val predator = Film("Predator", 1987, 7.9)
val dieHard = Film("Die Hard", 1988, 8.3)
val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6)
val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8)

val eastwood = Director("Clint", "Eastwood", 1930, Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))
val mcTiernan = Director("John", "McTiernan", 1951, Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))
val nolan = Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight, inception))
val someGuy = Director("Just", "Some Guy", 1990, Seq())
val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

assert((for (f <- nolan.films) yield f.name) == Seq("Memento", "Dark Knight", "Inception"))

val allFilms = for {
  director <- directors
  film <- director.films
} yield film.name
assert(allFilms == Seq("High Plains Drifter",
  "The Outlaw Josey Wales",
  "Unforgiven",
  "Gran Torino",
  "Invictus",
  "Predator",
  "Die Hard",
  "The Hunt for Red October",
  "The Thomas Crown Affair",
  "Memento",
  "Dark Knight",
  "Inception"))

val sortedByRating = (for {
  director <- directors
  film <- director.films
} yield film).sortWith(_.imdbRating > _.imdbRating)
assert(sortedByRating == Seq(Film("Dark Knight",2008,9.0),
  Film("Inception",2010,8.8),
  Film("Memento",2000,8.5),
  Film("Unforgiven",1992,8.3),
  Film("Die Hard",1988,8.3),
  Film("Gran Torino",2008,8.2),
  Film("The Outlaw Josey Wales",1976,7.9),
  Film("Predator",1987,7.9),
  Film("High Plains Drifter",1973,7.7),
  Film("The Hunt for Red October",1990,7.6),
  Film("Invictus",2009,7.4),
  Film("The Thomas Crown Affair",1999,6.8)))

for {
  director <- directors
  film <- director.films
} println(s"Tonight Only! ${film.name} by ${director.firstName} ${director.lastName}")