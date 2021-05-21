package monet

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.document

import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.dom.raw.CanvasRenderingContext2D
import org.scalajs.dom.raw.Event
import org.scalajs.dom.Element
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.raw.SVGElement
import org.scalajs.dom.raw.Document
import scala.collection.mutable.ArrayBuffer

import Layers._
import PixelLayerFunctions._

extension [A,B](f: A => B)
  def |>[C](g: B => C) : A => C = (a: A) => g(f(a))

object Main:
  import math.sqrt
  import PixelLayerFunctions._

  def main(args: Array[String]): Unit =
    document.addEventListener(
      "DOMContentLoaded",
      (e: Event) => render
    )

  def txt(text: String, classes: String*): Element =
    val e = document.createElement("p")
    e.textContent = text
    for (c <- classes) e.withClass(c)
    e

  // TODO: ultimately we will want to incorporate the monaco editor with some light scala-like parsing features (via: fastparse? scalameta?) and bidirectional editing functions.
  def render =
    // threejs()

    given Document = document
    given SVGContext = new SVGContext()

    document.body.withClass("center")
    // TODO: text layer API, something better for specifying classes.

    // val p = txt("MONET", "spaced")

    var w = dom.window.innerWidth
    var h = dom.window.innerHeight

    // TODO: port the reactive layer from mimic so we can just tweak the values directly.
    // TODO: we want something like the VectorLayer API here
    // TODO: add an API for clipping that gets compiled to clip-path properties on the canvas object
    // TODO: add an API for opacity
    val pixlayer = PixelLayer {
      val circPoints = Seq((250, 250, 0.1), (750, 750, 0.1))

      val cx = 250.0; val cy = 250.0
      val r = Percent(10)

      // Find the tangent from the border point to the circle.
      // TODO: add a web-assembly based 2d geometry library
      def targetPoint(sx: Double, sy: Double) =
        val dx = sx - cx; val dy = sy - cy
        val (dxr, dyr) = (-dy, dx)
        val d = sqrt(dx * dx + dy * dy)
        if (d >= r)
          val rho = r / d
          val ad = rho * rho
          val bd = rho * sqrt(1 - rho * rho)
          val t1x = cx + ad * dx + bd * dxr
          val t1y = cy + ad * dy + bd * dyr
          val t2x = cx + ad * dx - bd * dxr
          val t2y = cy + ad * dy - bd * dyr
          (t1x, t1y)
        else
          (sx, sy)

      def radialLine(sx: Double, sy: Double) =
        moveTo(sx, sy)
        val (tx, ty) = targetPoint(sx, sy)
        lineTo(tx, ty)

      for (t <- 0 until 100)
        val dt = t / 100.0
        radialLine(dt * w, 0)
        radialLine(w, dt * h)
        radialLine((1.0 - dt) * w, h)
        radialLine(0, (1.0 - dt) * h)

      lineWidth(0.5)
      strokeStyle("#aaa")
      stroke()
    }

    // TODO: figure out how extracting the objects for a cross-layer API might work --
    //       i.e. collect the created objects from the execution context.
    val veclayer = VectorLayer {
      // TODO: get a better color API. strings can be colors, but so can... you know, more descriptive representations.
      val g = RadialGradient("rg1", 0 -> "#9b78ff", 100 -> "#51c9e2")

      val mask = Mask("testMask", e =>
        e.child(svg("circle")
          .attr(
            "cx" -> w/2,
            "cy" -> h/2,
            "r" -> "20%",
            "fill" -> "black"))
      )

      def mkcirc(x: Int, y: Int) =
        var circPt = Pt(x, y)
        // TODO: parametric percentages are a function of width.
        Circle(circPt, "10%", g)
          .draggable
          .mask(mask)

      mkcirc(250, 250)
      mkcirc(750, 750)
    }

    val tweakLayer = VectorLayer {

      // Here's what a basic draggable interface looks like. We create
      // a set of initial points, make a path out of them, and create a
      // set of "vertices" which, when dragged, update the corresponding
      // point in the array and cause the path to re-render.
      val pts = ArrayBuffer(Pt(100,100),Pt(200,200),Pt(150,200),Pt(100,150))
      val path = Path(pts)

      for ((pt,i) <- pts.zipWithIndex)
        val c = Circle(pt,"5px","transparent").draggable((p : Pt) =>
          pts(i) = p
          path.update(pts)
        )
        c.withClass("handle")
          .attr("stroke","black")

      // However, that results an arbitrarily shaped path, not maintaining
      // any of the programmatic constraints of the initial shape. Instead
      // we will take a parameterized program and attempt to find the closest
      // parameter value to our target.
      given ctx: DiffContext = new DiffContext()
      def square(base : Pt, r: Diff) : ArrayBuffer[(Diff,Diff)] =
        ArrayBuffer(
          (base.x,  base.y),
          (base.x+r,base.y),
          (base.x+r,base.y+r),
          (base.x,  base.y+r))

      val base = Pt(500,100)
      val radius = 100.v
      val sqPts = square(base,radius)
      val concretePts = sqPts.map((a,b) => Pt(a.primal,b.primal))
      val sqPath = Path(concretePts)
      for ((pt,i) <- concretePts.zipWithIndex)
        // At this point our problem setup is nearly complete. We
        // will minimize the distance between the point
        // the target as the target is dragged. We do this by computing
        // the derivative of the expression and and then minimizing
        // along that line.
        val c = Circle(pt,"5px","transparent").draggable((target : Pt) =>
          val (dx,dy) = sqPts(i)
          val distX = dx - target.x
          val distY = dy - target.y
          val dist = (distX*distX) + (distY*distY)
          val Seq(dr) = dist.d(Seq(radius),0.001)
          // println(s"Original: $pt -> Target:$target (dr = $dr)")

          var iters = 0
          var prevDist = dist.primal
          ctx.update(radius -> (radius.primal - dr))
          while (prevDist > dist.primal && iters < 100)
            prevDist = dist.primal
            ctx.update(radius -> (radius.primal - dr))
            iters += 1
          // println(s"updated in ${iters} iterations (r = ${radius.primal})")
          sqPath.update(sqPts.map((a,b) => Pt(a.primal,b.primal)))
        )
        c.withClass("handle")
          .attr("stroke","black")
    }

    // TODO: add three.js based 3d layers that handle the boilerplate currently present in
    // the `boxes` (blend) and `points` (select) demos.
    val layers : Seq[Layer] = Seq(tweakLayer)

    // TODO: move the layer utilities elsewhere and allow us just to specify the layer sequence here.

    // When the canvas is resized we need to tweak.
    def setLayerSize(layer: Layer): Layer =
      layer
        .attr("width", w.toInt)
        .attr("height", h.toInt)

    def drawLayer(layer: Layer): Layer =
      layer.draw(w, h)

    dom.window.addEventListener(
      "resize",
      (_: Event) =>
        w = dom.window.innerWidth
        h = dom.window.innerHeight
        layers.map(setLayerSize |> drawLayer)
    )

    // document.body.appendChild(p)
    layers.map(layer =>
      layers.map(setLayerSize).map(drawLayer)
      document.body.appendChild(layer.element)
    )


