package com.nullhappens.learning

import doodle.core._
import doodle.turtle.Instruction._
import doodle.turtle._

object TurtleGraphics {

  def polygon(sides: Int, sideLength: Double): Image = {
    val step = Angle.one / sides
    def loop(n:Int): List[Instruction] =
      n match {
        case 0 => Nil
        case n => turn(step) :: forward(sideLength) :: loop(n - 1)
      }
    Turtle.draw(loop(sides))
  }

}