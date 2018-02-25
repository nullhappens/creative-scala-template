package com.nullhappens.learning

import doodle.core.Image.Triangle
import doodle.core._

object Recursion {
  val aBox: Image = Image.rectangle(20, 20).fillColor(Color.royalBlue)

  def boxes(count: Int): Image =
    count match {
      case 0 => Image.empty
      case n => aBox beside boxes(n - 1)
    }

  def cross(count: Int): Image = {
    val aCircle: Image = Image.circle(20.0)
    count match {
      case 0 => aCircle
      case n => aCircle beside (aCircle above cross(n - 1) above aCircle) beside aCircle
    }
  }

  def chessboard(count: Int): Image = {
    def rect(color: Color): Image = Image.rectangle(30, 30).fillColor(color)
    val base: Image =
      (rect(Color.red) beside rect(Color.black)) above (rect(Color.black) beside rect(Color.red))
    count match {
      case 0 => base
      case n =>
        val unit: Image = chessboard(n - 1)
        (unit beside unit) above (unit beside unit)
    }
  }

  def sierpinski(count: Int, len: Int): Image = {
    val triangle: Image = Triangle(len, len) lineColor Color.royalBlue
    def base: Image = triangle above (triangle beside triangle)
    count match {
      case 0 => base
      case n =>
        val unit = sierpinski(n -1, len)
        unit above (unit beside unit)
    }
  }

}