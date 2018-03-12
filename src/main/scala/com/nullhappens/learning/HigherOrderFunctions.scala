package com.nullhappens.learning

import doodle.core._

object HigherOrderFunctions {

  def parametricCircle(angle: Angle): Point =
    Point.cartesian(angle.cos * 200, angle.sin * 200)

  def rose(angle: Angle): Point =
    Point.polar((angle * 7).cos * 200, angle)

  def sampleCircle(samples: Int): Image = {
    val step = Angle.one / samples
    val dot = Image.triangle(10, 10)
    def loop(count: Int): Image = {
      val angle = step * count
      count match {
        case 0 => Image.empty
        case n => dot.at(parametricCircle(angle).toVec) on loop(n - 1)
      }
    }
    loop(samples)
  }
}
