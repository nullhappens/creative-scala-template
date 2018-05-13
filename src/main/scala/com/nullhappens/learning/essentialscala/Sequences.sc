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

def filterDirectorsByNumberOfFilms(value: Int): Seq[Director] =
  directors.filter(_.films.length > value)

assert(filterDirectorsByNumberOfFilms(4) == Seq(eastwood))

def filterDirectorsByYearOfBirth(value: Int): Seq[Director] =
  directors.filter(_.yearOfBirth < value)

assert(filterDirectorsByYearOfBirth(1990) == Seq(eastwood, mcTiernan, nolan))

def filterDirectorsByYearAndFilms(year: Int, filmCount: Int): Seq[Director] =
  filterDirectorsByNumberOfFilms(filmCount) intersect filterDirectorsByYearOfBirth(year)

assert(filterDirectorsByYearAndFilms(1990, 4) == Seq(eastwood))

def sortDirectors(ascending: Boolean): Seq[Director] =
  if (ascending)
    directors.sortWith(_.yearOfBirth > _.yearOfBirth)
  else
    directors.sortWith(_.yearOfBirth < _.yearOfBirth)

assert(sortDirectors(true) == Seq(someGuy, nolan, mcTiernan, eastwood))
assert(sortDirectors(false) == Seq(eastwood, mcTiernan, nolan, someGuy))

assert(nolan.films.map(_.name) == Seq("Memento", "Dark Knight", "Inception"))
assert(directors.flatMap(_.films.map(_.name)) ==
  Seq("High Plains Drifter",
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

assert(mcTiernan.films.sortWith(_.yearOfRelease < _.yearOfRelease).head == Film("Predator", 1987, 7.9))

assert(directors.flatMap(_.films).sortWith(_.imdbRating > _.imdbRating) ==
  Seq(Film("Dark Knight",2008,9.0),
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

assert(
  directors.flatMap(_.films).foldLeft(0.0)((acc, f) => acc + f.imdbRating) / directors.flatMap(_.films).length ==
  directors.flatMap(_.films.map(_.imdbRating)).sum / 12)

directors.foreach(d => d.films.foreach(f => println(s"Tonight only! ${f.name} by ${d.firstName} ${d.lastName}")))

//earliest film by any director
directors.flatMap(_.films).sortWith(_.yearOfRelease < _.yearOfRelease).headOption

def smallest(list: Seq[Int]): Option[Int] = list.sortWith(_ < _).headOption

assert(smallest(Nil).isEmpty)
assert(smallest(Seq(3,2,1)).contains(1))

def unique(list: Seq[Int]): Seq[Int] = list.foldLeft(Seq.empty[Int]){(acc, el) =>
  if(!acc.contains(el)) acc :+ el else acc
}

assert(unique(Seq(1, 1, 2, 4, 3, 4)) == Seq(1,2,4,3))

def reverse(list: Seq[Int]): Seq[Int] = list.foldLeft(Seq.empty[Int])((acc, el) => el +: acc)

assert(reverse(Seq(1,2,3)) == Seq(3,2,1))

def mapWithFold[A,B](list: Seq[A], f: A => B): Seq[B] = list.foldLeft(Seq.empty[B])(_ :+ f(_))

assert(mapWithFold[Int, Int](Seq(1,2,3), x => x * 2) == Seq(2,4,6))
