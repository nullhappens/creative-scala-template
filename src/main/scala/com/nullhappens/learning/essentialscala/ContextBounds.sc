trait HtmlWriter[A] {
  def write(in: A): String
}
implicit class HtmlOps[T](data: T){
  def toHtml(implicit writer: HtmlWriter[T]) = writer.write(data)
}

def pageTemplate[A : HtmlWriter](body: A): String =
  // this is equal to writing
  // def pageTemplate[A](body: A)(implicit writer: HtmlWriter[A]): String
  s"<html><head></head><body>${body.toHtml}</body></html>"


