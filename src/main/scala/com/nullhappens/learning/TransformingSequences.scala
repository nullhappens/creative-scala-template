package com.nullhappens.learning

object TransformingSequences {

  def myMap[A, B](xs: List[A], fn: A => B):List[B] =
    xs match {
      case Nil => Nil
      case hd :: tail => fn(hd) :: myMap(tail, fn)
    }

  def fill[A](n:List[Int], a:A):List[A] = n.map(_ => a)

  def fill[A](n:Int, a:A): List[A] = (0 until n).map(_ => a).toList

  def ones(n:Int):List[Int] = fill(n, 1)

  def descending(n:Int):List[Int] = (n until 0 by -1).map(x => x).toList

  def ascending(n:Int):List[Int] = (0 until n).map(x => x + 1).toList

  def double(xs:List[Int]):List[Int] = xs.map(x => x * 2)
  
}
