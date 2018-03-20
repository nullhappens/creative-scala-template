package com.nullhappens.learning.creativescala

import doodle.core.Image
import doodle.turtle.Instruction
import doodle.turtle.Instruction._
import doodle.syntax._
import doodle.core.Angle
import doodle.turtle._

object FlatMap {

  def double[A](lst: List[A]): List[A] =
    lst.flatMap{
      x => List(x, x)
    }

  def nothing[A](lst: List[A]): List[A] =
    lst.flatMap{
      _ => List()
    }

  def rewrite(instructions: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] = {
    instructions.flatMap{ i =>
      i match {
        case Branch(i) => List(branch(rewrite(i, rule):_*))
        case other => rule(other)
      }
    }
  }

  def iterate(steps: Int, seed: List[Instruction], rule: Instruction => List[Instruction]): List[Instruction] = {
    steps match {
      case 0 => seed
      case n => iterate(n - 1, rewrite(seed, rule), rule)
    }
  }

  def lSystem(): Image = {
    // TODO: We'll come back to this later
    Image.empty
  }

  def polygon(sides: Int, sideLength: Double): Image = {
    val rotation = Angle.one / sides
    Turtle.draw((0 to sides).toList.flatMap{
      _ => List(forward(sideLength), turn(rotation))
    })
  }

  def flatSpiral(steps: Int, distance: Double, angle: Angle, increment: Double): Image = {
    val instructions = (1 to steps).toList.flatMap {
      n => List(forward(distance + (increment * n)), turn(angle + increment.degrees))
    }
    Turtle.draw(instructions)
  }
}
