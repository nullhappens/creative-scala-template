package com.nullhappens.learning.creativescala

import doodle.core.{Angle, Color, Normalized, Image}
import doodle.random._
import doodle.syntax._

object GenerativeArt {

  val randomAngle: Random[Angle] = Random.double map(x => x.turns)

  def randomColor(s: Normalized, l: Normalized): Random[Color] =
    randomAngle map (hue => Color.hsl(hue, s, l))

  def randomColor: Random[Color] =
    randomAngle map (hue => Color.hsl(hue, 0.7.normalized, 0.7.normalized))

  def randomCircle(r: Double, color: Random[Color]): Random[Image] =
    color map (fill => Image.circle(r) fillColor fill)

  def randomSquare(r: Double, color: Random[Color]): Random[Image] =
    color map(fill => Image.rectangle(r, r) fillColor fill)

  def coloredRectangle(color: Color, size: Int): Image =
    Image.rectangle(size, size).
      lineWidth(5).
      lineColor(color.spin(30.degrees)).
      fillColor(color)

  def coloredRectangle(color: Color): Image = coloredRectangle(color, 20)

  def randomConcentricCircles(count: Int, size: Int): Random[Image] = {
    val randomPastel = randomColor(0.7.normalized, 0.7.normalized)
    count match {
      case 0 => Random.always(Image.empty)
      case n => randomCircle(size, randomPastel) flatMap {
        circle => randomConcentricCircles(n - 1, size + 5) map {
          circles => circle on circles
        }
      }
    }
  }

  def randomColorBoxes(count: Int): Random[Image] = {
    val box = randomColor map(c => coloredRectangle(c))
    count match {
      case 0 => Random.always(Image.empty)
      case n =>
        val boxes = randomColorBoxes(n - 1)
        box flatMap {
          b => boxes map {
            bs => b beside bs
          }
        }
    }
  }

  def nextColor(color: Color): Random[Color] = {
    val spin = Random.normal(15, 10)
    spin map { s => color.spin(s.degrees)}
  }

  def randomGradientBoxes(count: Int, color: Color): Random[Image] = {
    val box = coloredRectangle(color, 20)
    count match {
      case 0 => Random.always(Image.empty)
      case n =>
        val boxes = nextColor(color) flatMap {
          c => randomGradientBoxes(n - 1, c)
        }
        boxes map { b => box beside b }
    }
  }

}
