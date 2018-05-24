import java.util.Date

// Our DSL

sealed trait Visitor {
  def id: String
  def createdAt: Date
  def age: Long = new Date().getTime - createdAt.getTime
}

final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor
final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor


// Our JSON writer definitions

sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  override def stringify: String = values.map{ case (name, value) =>
    s"""
       |"$name": ${value.stringify}
     """.stripMargin.trim
  }.mkString("{", ",", "}")
}

final case class JsString(value: String) extends JsValue {
  override def stringify: String =
    s"""
       |"${value.replaceAll("\\|\"", "\\\\$1")}"
     """.stripMargin.trim
}

val json = JsObject(Map("foo" -> JsString("a"), "bar" -> JsString("b"), "baz" -> JsString("c")))
json.stringify

trait JsWriter[A] {
  def write(value: A): JsValue
}

implicit class JsUtil[A](value: A){
  def toJson(implicit writer: JsWriter[A]) = writer.write(value)
}

implicit object AnonymousWriter extends JsWriter[Anonymous] {
  override def write(value: Anonymous): JsValue = JsObject(Map(
    "id" -> JsString(value.id),
    "createdAt" -> JsString(value.createdAt.toString)
  ))
}

implicit object UserWriter extends JsWriter[User] {
  override def write(value: User): JsValue = JsObject(Map(
    "id" -> JsString(value.id),
    "email" -> JsString(value.email),
    "createdAt" -> JsString(value.createdAt.toString)
  ))
}

implicit object VisitorWriter extends JsWriter[Visitor] {
  override def write(value: Visitor): JsValue = value match {
    case anon: Anonymous => anon.toJson
    case user: User => user.toJson
  }
}

// lets test this shizzle

val visitors: Seq[Visitor] = Seq(Anonymous("001", new Date), User("003", "dave@example.com", new Date))
visitors.map(_.toJson)
