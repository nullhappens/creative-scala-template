package com.nullhappens.learning.creativescala

import doodle.core._

object Recursion {

  def boxes(count: Int): Image = {
    val unit: Image = Image.rectangle(20, 20).fillColor(Color.royalBlue)
    def loop(count: Int): Image =
      count match {
        case 0 => Image.empty
        case n => unit beside loop(n - 1)
      }
    loop(count)
  }

  def cross(count: Int): Image = {
    val unit: Image = Image.circle(20.0)
    count match {
      case 0 => unit
      case n =>
        unit beside (unit above cross(n - 1) above unit) beside unit
    }
  }

  def chessboard(count: Int): Image = {
    def rect(color: Color): Image = Image.rectangle(30, 30).fillColor(color)
    val base: Image = (rect(Color.red) beside rect(Color.black)) above (rect(
      Color.black) beside rect(Color.red))
    def loop(count: Int): Image =
      count match {
        case 0 => base
        case n =>
          val unit: Image = loop(n - 1)
          (unit beside unit) above (unit beside unit)
      }
    loop(count)
  }

  def sierpinski(count: Int, len: Int): Image = {
    val triangle: Image = Image.triangle(len, len) lineColor Color.royalBlue
    def base: Image = triangle above (triangle beside triangle)
    def loop(count: Int): Image =
      count match {
        case 0 => base
        case n =>
          val unit = loop(n - 1)
          unit above (unit beside unit)
      }
    loop(count)
  }

  def gradientBoxes(count: Int, color: Color): Image = {
    count match {
      case 0 => Image.empty
      case n =>
        Image.rectangle(20, 20) fillColor color beside gradientBoxes(
          n - 1,
          color.spin(Angle.degrees(5 * n)))
    }
  }

  def concentricCircle(count: Int, color: Color): Image = {
    def unit(size: Double, color: Color): Image =
      Image.circle(size) lineColor color lineWidth 3.0
    def loop(count: Int): Image = count match {
      case 0 => Image.empty
      case n => unit(20.0 * n, color) on loop(n - 1)
    }
    loop(count)
  }
}
