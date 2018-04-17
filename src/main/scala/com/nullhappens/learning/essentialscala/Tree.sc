sealed trait Tree[A] {
  def fold[B](node: (B, B) => B, leaf: A => B): B
}
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def fold[B](node: (B, B) => B, leaf: A => B): B =
    node(left.fold(node, leaf), right.fold(node, leaf))
}

case class Leaf[A](value: A) extends Tree[A] {
  override def fold[B](node: (B, B) => B, leaf: A => B): B =
    leaf(value)
}

val tree: Tree[String] = Node(Node(Leaf("To"), Leaf("iterate")),
  Node(Node(Leaf("is"), Leaf("human")),
    Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

tree.fold[String]((a, b) => s"$a $b", str => str)
