package com.nullhappens.learning.creativescala

import doodle.core._
import doodle.syntax._
import doodle.core.Image._
import doodle.core.PathElement._
import doodle.core.Point._

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

  def ascending(n:Int):List[Int] = (1 to n).map(x => x).toList

  def double(xs:List[Int]):List[Int] = xs.map(x => x * 2)


  def star(sides: Int, skip: Int, radius: Double): Image = {
    val rotation = 360.degrees * skip / sides
    val start = moveTo(polar(radius, 0.degrees));
    val elements = (1 until sides).toList.map {
      index => val point = polar(radius, rotation * index)
        lineTo(point)
    }
    closedPath(start :: elements) lineWidth 2.0
  }

  def allBeside(images: List[Image]): Image = {
    images match {
      case Nil => Image.empty
      case head :: tail => head beside allBeside(tail)
    }
  }

  def allBesideRun(): Image =
    allBeside((1 to 5).toList.map{
      skip => star(11, skip, 100)
    })

  def polygon(sides: Int, size: Int, initialRotation: Angle): Image = {
    val step = (Angle.one / sides).toDegrees
    val path = (0.0 to 360.0 by step).map{
      deg => lineTo(polar(size, initialRotation + deg.degrees))
    }.toList

    closedPath(moveTo(polar(size, initialRotation)) :: path)
  }

  def polygons(): Image = {
    val list = (3 to 40).map(x => polygon(x, x * 10, 0.degrees)).toList
    def combine(lst: List[Image]): Image = {
      lst match {
        case Nil => Image.empty
        case hd :: tl => hd on combine(tl)
      }
    }
    combine(list)
  }

  def polygons2(): Image = {
    (3 to 40).map(x => polygon(x, x * 10, 0.degrees)).toList.fold(Image.empty)((i1, i2) => i1 on i2)
  }
}
