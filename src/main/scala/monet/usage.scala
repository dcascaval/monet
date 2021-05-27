package monet

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.document

import typings.nlopt._
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


// These have to exist for the arity-1 case of `Program` to work, otherwise
// the Scala compiler crashes -- unclear why, as T is not a subtype of Tuple
given soloTupler[A] : Conversion[A,Tuple1[A]] =
  a => Tuple1(a)

given tupler[A,B] : Conversion[Function1[A,B],Function1[Tuple1[A],B]] =
  (f) => (k: Tuple1[A]) => f(k._1)


object Main:
  import math.sqrt
  import PixelLayerFunctions._

  def main(args: Array[String]): Unit =
    document.addEventListener(
      "DOMContentLoaded",
      (e: Event) => nlopt.ready.`then`((_) => render)
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
        var circPt = Pt[Double](x, y)
        // TODO: parametric percentages are a function of width.
        Circle(circPt, "10%", g)
          .draggable
          .mask(mask)

      mkcirc(250, 250)
      mkcirc(750, 750)
    }

    val tweakLayer = VectorLayer {
      val g = RadialGradient("rg1", 0 -> "#9b78ff", 100 -> "#51c9e2")

      // Here's what a basic draggable interface looks like. We create
      // a set of initial points, make a path out of them, and create a
      // set of "vertices" which, when dragged, update the corresponding
      // point in the array and cause the path to re-render.
      val pts = ArrayBuffer[Pt[Double]]
        (Pt(100,100),Pt(200,200),Pt(150,200),Pt(100,150))
      val path = Path(pts.toSeq)

      for ((pt,i) <- pts.zipWithIndex)
        val c = Circle(pt,"5px","transparent").draggable((p : Pt[Double]) =>
          pts(i) = p
          path.update(pts.toSeq)
        )
        c.withClass("handle")
          .attr("stroke","black")

      // However, that results an arbitrarily shaped path, not maintaining
      // any of the programmatic constraints of the initial shape. Instead
      // we will take a parameterized program and attempt to find the closest
      // parameter value to our target.
      given ctx: DiffContext = new DiffContext()
      def square(base : Pt[Double], r: Diff) : Seq[Pt[Diff]] =
        Seq(
          Pt[Diff](base.x,  base.y),
          Pt[Diff](base.x+r,base.y),
          Pt[Diff](base.x+r,base.y+r),
          Pt[Diff](base.x,  base.y+r))

      val base = Pt[Double](500,100)
      val radius = 100.v
      var sqPts = square(base,radius)
      var concretePts = sqPts.map(_.map(_.primal))
      val sqPath = Path(concretePts)
      sqPath.attr("fill","black")

      var circles : Seq[Circle] = null
      circles = concretePts.zipWithIndex.map((pt,i) =>
        // At this point our problem setup is nearly complete. We
        // will minimize the squared distance between the point
        // the target as the target is dragged. We do this by computing
        // the derivative of the expression and and then minimizing
        // along that line.
        val c = Circle(pt,"5px","transparent").draggable((target : Pt[Double]) =>
          val Pt(dx, dy) = sqPts(i)
          val (distX, distY) = (dx - target.x, dy - target.y)
          val dist = (distX * distX) + (distY * distY)
          ctx.prepare(Seq(dist))
          var prevDist = dist.primal

          // Alternatively iterate to find the appropriate new radius. (a "10-shot") constant approximation, which seems to work quite well in simple cases.
          val Seq(dr) = dist.d(Seq(radius), 0.1)
          var iters = 0
          ctx.update(radius -> (radius.primal - dr))
          while (prevDist > dist.primal && iters < 10)
            prevDist = dist.primal
            ctx.update(radius -> (radius.primal - dr))
            iters += 1

          // Update the path position and the vertex positions
          sqPts = square(base, radius)
          concretePts = sqPts.map(_.map(_.primal))
          sqPath.update(concretePts)
          for ((newPt,j) <- concretePts.zipWithIndex if j != i)
            circles(j).setPosition(newPt)
        )
        c.withClass("handle")
         .attr("stroke","black")
        c
      )

      // Now we need a way of programmatically generating the above function. One way is to say, this is always
      // the optimization strategy we're going to use, it's
      // going to be equivalent for each point. So, rewriting:

      val r2 = (150.v)
      val base2 = Pt[Double](100,300)

      // arity 1
      val execute = (r: Diff) => square(base2, r)
      Program(r2, execute,
        (_,pts) => { val p = Path(pts); p.attr("fill","#228B22"); Seq(p) },
        (_,pts,e) => { val Seq(p) = e; p.update(pts) }
      )

      // Great, this works. Let's try a circle?

      val base3 = Pt[Double](500, 450)
      val (r,t) = (100.v, 0.v)



      val circleFn = (r: Diff, t: Diff) =>
        val iters = 12
        (0 until iters).map(i =>
          val p = i.toDouble / iters
          val theta = (t/360.0) + p * (2*math.Pi)
          Pt[Diff](base3.x + r * theta.cos, base3.y + r * theta.sin)
        )

      val p1 = Program((r,t), circleFn.tupled,
        { case ((r,_),_) => Seq(Circle(base3,s"${r.toInt}px",g)) },
        { case ((r,_),_,s) => s(0).attr("r",r.primal.toInt) }
      )()

      val p2 = Program((r,t), circleFn.tupled,
        (_,pts) => pts.map(p => Circle(p,"10px",g)) ,
        (_,pts,geos) => pts.zip(geos).map((p,e) => e.setPosition(p))
      )()

      // Now we need a reasonable abstraction for composing shapes, i.e. a multi-path one that
      // allows re-use of parameters and generation of paths through operations such as `Mirror`,
      // `Translate`, `Rotate`, and so on.

      // This is an attempt at such:
      val axis : Axis[Diff] = Axis(Pt(0,250),Pt(100,250))
      val p3 = Mirror(p2, axis)()


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


