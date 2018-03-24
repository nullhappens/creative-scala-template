package com.nullhappens.learning.creativescala

import doodle.core.{Point, Angle, Normalized, Image, Color}
import doodle.random._
import doodle.syntax._

object ParametricNoise {

  def perturb(point: Point): Random[Point] = {
    for {
      x <- Random.normal(0, 10)
      y <- Random.normal(0, 10)
    } yield Point.cartesian(point.x + x, point.y + y)
  }
  def scale(factor: Double): Point => Point =
    (pt: Point) => Point.polar(pt.r * factor, pt.angle)

  def rose(k: Int): Angle => Point =
    (angle: Angle) => {
      val x = (angle * k).cos * angle.cos
      val y = (angle * k).cos * angle.sin
      Point.cartesian(x, y)
    }

  def perturbedRose(k: Int): Angle => Random[Point] =
    rose(k) andThen perturb


  //This monstrosity of a function stack overflows
  // FIXME: fix that when I get better at this
  def smoke(r: Normalized): Random[Image] = {
    val alpha = Random.normal(0.5, 0.1).map(a => a.normalized)
    val hue = Random.double.map(h => (h * 0.1).turns)
    val saturation = Random.double.map(s => (s * 0.8).normalized)
    val lightness = Random.normal(0.4, 0.1) map(a => a.normalized)
    val color =
      for {
        h <- hue
        s <- saturation
        l <- lightness
        a <- alpha
      } yield Color.hsla(h,s,l,a)
    val c = Random.normal(5, 5).map(r => Image.circle(r))
    for {
      circle <- c
      line <- color
    } yield circle.lineColor(line).noFill

    def point(position: Angle => Point, scale: Point => Point, perturb: Point => Random[Point], image: Normalized => Random[Image], rotation: Angle): Angle => Random[Image] = {
      (angle: Angle) => {
        val pt = position(angle)
        val scaledPt = scale(pt)
        val perturbed = perturb(scaledPt)
        val r = pt.r.normalized
        val img = image(r)
        for {
          i <- img
          pt <- perturbed
        } yield i at pt.toVec.rotate(rotation)
      }
    }

    def iterate(step: Angle): (Angle => Random[Image]) => Random[Image] = {
      (point: Angle => Random[Image]) => {
        def iter(angle: Angle): Random[Image] = {
          if (angle > Angle.one)
            Random.always(Image.empty)
          else
            for {
              p <- point(angle)
              ps <- iter(angle + step)
            } yield p on ps
        }
        iter(Angle.zero)
      }
    }

    val image: Random[Image] = {
      val pts = for(i <- 20 to 360 by 39) yield {
        iterate(1.degrees){
          point(rose(5), scale(i), perturb _, smoke _, i.degrees)
        }
      }
      val picture = pts.foldLeft(Random.always(Image.empty)) {
        (accum, img) =>
          for {
            a <- accum
            i <- img
          } yield a on i
      }
      val background = Image.rectangle(650, 650) fillColor Color.black
      picture map { _ on background }
    }
    image
  }
}
