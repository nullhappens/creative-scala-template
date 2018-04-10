import java.util.Date

sealed trait Visitor {
  def id:String
  def createdAt:Date
  def age:Long = new Date().getTime - createdAt.getTime
}

final case class Anonymous(id:String, createdAt:Date = new Date()) extends Visitor

final case class User(id:String, email:String, createdAt:Date = new Date()) extends Visitor

val anonymous = Anonymous("anon1")
anonymous.createdAt
anonymous.age

sealed trait Feline {
  def color:String
  def sound:String
}
sealed trait BigCat extends Feline {
  def sound:String = "roar"
}

final case class Cat(name:String, color:String, food:String) extends Feline {
  def sound:String = "meow"
}
final case class Tiger(name:String, color:String) extends BigCat
final case class Panther(name:String, color:String) extends BigCat
final case class Lion(name:String, color:String, maneSize:Int) extends BigCat

sealed trait Shape {
  def sides:Int
  def perimeter:Double
  def area:Double
  def color:Color
}

sealed trait Rectangular extends Shape {
  def width:Double
  def height:Double
  val sides = 4
  val perimeter = 2*width + 2*height
  val area = width * height
}

sealed trait Color {
  def red:Double
  def green:Double
  def blue:Double
  def isLight:Boolean = (red + green + blue) / 3 > 128
  def isDark:Boolean = !isLight
}

final case object Yellow extends Color {
  val red = 255
  val green = 255
  val blue = 0
}

final case object Red extends Color {
  val red = 255
  val green = 0
  val blue = 0
}

final case object Pink extends Color {
  val red = 255
  val green = 192
  val blue = 203
}

final case class Custom(red:Double, green:Double, blue:Double) extends Color

object Draw {
  def apply(shape:Shape):String =
    shape match {
      case Circle(radius, color) =>
        s"A ${Draw(color)} circle with radius = $radius"
      case Square(width, color) =>
        s"A ${Draw(color)} square with width = $width"
      case Rectangle(width, height, color) =>
        s"A ${Draw(color)} rectangle with width = $width and height = $height"
    }

  def apply(color:Color):String =
    color match {
      case Yellow => "yellow"
      case Pink => "pink"
      case Red => "red"
      case other => if (other.isLight) "light" else "dark"
    }
}

final case class Circle(radius:Double, color: Color) extends Shape {
  val sides = 1
  val perimeter = 2 * math.Pi * radius
  val area = math.Pi * math.pow(radius, 2)
}
final case class Rectangle(width:Double, height:Double, color: Color) extends Rectangular

final case class Square(width:Double, color: Color) extends Rectangular {
  val height = width
}

Draw(Square(10, Pink))
Draw(Rectangle(10, 20, Red))
Draw(Circle(22, Custom(0, 255, 0)))

sealed trait DivisionResult
final case class Finite(value:Int) extends DivisionResult
final case object Infinite extends DivisionResult

object divide {
  def apply(numerator:Int, denominator:Int):DivisionResult =
    if (denominator == 0) Infinite else Finite(numerator / denominator)
}

divide(1, 0) match {
  case Infinite => "WOMP WOMP its infinite"
  case Finite(result) => s"Finite value $result"
}

