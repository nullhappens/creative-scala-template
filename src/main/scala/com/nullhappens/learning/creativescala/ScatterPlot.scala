package com.nullhappens.learning.creativescala

import doodle.random._
import doodle.syntax._
import doodle.core.{Image, Point, Color}
import com.nullhappens.learning.creativescala.GenerativeArt._

object ScatterPlot {

  def randomConcentricCircles(count: Int, size: Int): Random[Image] =
    count match {
      case 0 => Random.always(Image.empty)
      case n => for {
          circle <- randomCircle(size, randomColor)
          circles <- randomConcentricCircles(n -1, size + 5)
        } yield circle on circles
    }

  def makePoint(x: Random[Double], y: Random[Double]): Random[Point] =
    for {
      xVal <- x
      yVal <- y
    } yield Point.cartesian(xVal, yVal)

  def makePoints(count: Int): List[Random[Point]] = {
    val normal = Random.normal(50, 15)
    (1 to count).toList.map(_ => makePoint(normal, normal))
  }

  def point(loc: Point): Image =
    Image.circle(20).fillColor(Color.royalBlue.fadeOut(0.5.normalized)).noLine at loc.toVec

  def pointsToImages(lst: List[Random[Point]]): List[Random[Image]] =
    lst.map( r => r.map(point))

  def allOn(points: List[Random[Image]]): Random[Image] = {
    points match {
      case Nil => Random.always(Image.empty)
      case img :: imgs =>
        for {
          i <- img
          is <- allOn(imgs)
        } yield i on is
    }
  }


}
