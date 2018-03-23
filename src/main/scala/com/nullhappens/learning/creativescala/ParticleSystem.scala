package com.nullhappens.learning.creativescala

import doodle.random.Random
import doodle.core.{Color, Image, Point, Vec}
import doodle.syntax._

object ParticleSystem {

  val start = Random.always(Point.zero)

  def step(current: Point): Random[Point] = {
    val drift = Point(current.x + 10, current.y)
    val noise = Random.normal(0, 5) flatMap { x =>
        Random.normal(0, 5) map { y =>
          Vec(x, y)
        }
      }
    noise.map(vec => drift + vec)
  }
  // def render(point: Point): Image = Image.circle(10).noFill.lineWidth(2).at(point.toVec)

  def render(point: Point): Image = {
    val length = (point - Point.zero).length
    val hue = (length / 200).turns
    val color = Color.hsl(hue, 0.7.normalized, 0.5.normalized)
    Image.circle(10).noFill.lineColor(color).at(point.toVec)
  }

  def walk(steps: Int): Random[Image] = {
    def loop(count: Int, current: Point, image: Image): Random[Image] = {
      count match {
        case 0 => Random.always(image on render(current))
        case n =>
          val next = step(current)
          next flatMap { pt =>
            loop(count - 1, pt, image on render(current))
          }
      }
    }
    start flatMap { pt =>
      loop(steps, pt, Image.empty)
    }
  }

  def particleSystem(particles: Int, steps: Int): Random[Image] = {
    particles match {
      case 0 => Random.always(Image.empty)
      case n => walk(steps) flatMap{ img1 =>
        particleSystem(n - 1, steps) map { img2 =>
          img1 on img2
        }
      }
    }
  }
}
