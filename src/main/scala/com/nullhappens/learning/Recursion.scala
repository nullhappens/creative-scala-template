package com.nullhappens.learning
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

  
}