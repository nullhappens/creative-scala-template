package com.nullhappens.learning

object Lists {

  def sayHi(length: Int): List[String] =
    length match {
      case 0 => Nil
      case n => "Hi" :: sayHi(n - 1)
    }

  def increment(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case head :: tail => (head + 1) :: increment(tail)
    }

  def sum(xs: List[Int]): Int =
    xs match {
      case Nil => 0
      case head :: tail => head + sum(tail)
    }

  def length[A](xs: List[A]): Int =
    xs match {
      case Nil => 0
      case head :: tail => 1 + length(tail)
    }

  def ones(length: Int): List[Int] =
    length match {
      case 0 => Nil
      case n => 1 :: ones(n - 1)
    }

  def descending(length: Int): List[Int] =
    length match {
      case 0 => Nil
      case n => n :: descending(n - 1)
    }

  def ascending(length: Int): List[Int] = {
    def loop(n: Int, counter: Int): List[Int] =
      n match {
        case 0 => Nil
        case i => counter :: loop(i - 1, counter + 1)
      }
    loop(length, 1)
  }

  def fill[A](n: Int, a: A): List[A] =
    n match {
    case 0 => Nil
    case i => a :: fill(i - 1, a)
  }

  def double(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case hd :: tl => hd * 2 :: double(tl)
    }

  def product(xs: List[Int]): Int =
    xs match {
      case Nil => 1
      case hd :: tl => hd * product(tl)
    }

  def contains[A](xs: List[A], a: A): Boolean =
    xs match {
      case Nil => false
      case hd :: tl => (hd == a) || contains(tl, a)
    }

  def first[A](xs: List[A], a: A): A =
    xs match {
      case Nil => a
      case head :: tail => head
    }

  def reverse[A](xs: List[A]): List[A] = {
    def loop(lst: List[A], acc: List[A]): List[A] =
      lst match {
        case Nil => acc
        case hd :: tl => loop(tl, hd :: acc)
      }
    loop(xs, List())
  }

}
