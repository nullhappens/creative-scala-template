package com.nullhappens.learning.creativescala

import doodle.core._

object HigherOrder {

  def parametricCircle(angle: Angle): Point = Point.polar(200, angle)

  // Type Angle => Point
  def rose(angle: Angle): Point = Point.polar((angle * 7).cos * 200, angle)

  // Rose written as a function literal
  def roseFn = (angle: Angle) => Point.polar((angle * 7).cos * 200, angle)

  def sample(start: Angle, samples: Int): Image = {
    val step = Angle.one / samples
    val dot = Image.triangle(10, 10)

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n => dot.at(parametricCircle(angle).toVec) on loop(n - 1)
      }
    }

    loop(samples);
  }

  def sampleParametric(samples: Int, parametricFn: Angle => Point, dot: Image): Image = {
    val step = Angle.one / samples

    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n => dot.at(parametricFn(angle).toVec) on loop(n - 1)
      }
    }

    loop(samples);
  }

  def concentricShapes(count: Int, singleShape: Int => Image): Image =
    count match {
      case 0 => Image.empty
      case n => singleShape(n) on concentricShapes(n - 1, singleShape)
    }

}
