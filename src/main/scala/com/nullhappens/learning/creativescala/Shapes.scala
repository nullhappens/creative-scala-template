package com.nullhappens.learning.creativescala

import doodle.core._
import doodle.core.PathElement._
import doodle.core.Point._
import doodle.syntax._

object Shapes {

  val triangle: Image =
    Image.closedPath(List(
      lineTo(cartesian(50, 100)),
      lineTo(cartesian(100, 0)),
      lineTo(cartesian(0, 0))
    ))

  val square: Image =
    Image.closedPath(List(
      lineTo(cartesian(50, 0)),
      lineTo(cartesian(50, 50)),
      lineTo(cartesian(0, 50))
    ))

  def pentagon: Image =
    Image.closedPath(List(
      moveTo(polar(50, 72.degrees)),
      lineTo(polar(50, 144.degrees)),
      lineTo(polar(50, 216.degrees)),
      lineTo(polar(50, 288.degrees)),
      lineTo(polar(50, 360.degrees))
    ))

}
