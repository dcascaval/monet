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
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.raw.SVGElement
import org.scalajs.dom.raw.Document
import scala.collection.mutable.ArrayBuffer

import Layers._
import PixelLayerFunctions._

extension [A,B](f: A => B)
  def |>[C](g: B => C) : A => C = (a: A) => g(f(a))

def dbg[T](expr: => T): T =
  val result = expr
  println(result.toString)
  result

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

      // // Here's what a basic draggable interface looks like. We create
      // // a set of initial points, make a path out of them, and create a
      // // set of "vertices" which, when dragged, update the corresponding
      // // point in the array and cause the path to re-render.
      // val pts = ArrayBuffer[Pt[Double]]
      //   (Pt(100,100),Pt(200,200),Pt(150,200),Pt(100,150))
      // val path = Path(pts.toSeq)

      // for ((pt,i) <- pts.zipWithIndex)
      //   val c = Circle(pt,"5px","transparent").draggable(p =>
      //     pts(i) = p
      //     path.update(pts.toSeq)
      //   )
      //   c.withClass("handle")
      //     .attr("stroke","black")
      given ctx: DiffContext = new DiffContext()


      val trapBase = Pt[Double](950,150)
      val trapezoidFn = (w: Diff) =>
        val p1 = Pt(trapBase.x-w,trapBase.y-w)
        Seq[Pt[Diff]](
          p1,
          Pt(trapBase.x+w,trapBase.y+w),
          Pt(trapBase.x,  trapBase.y+w),
          Pt(trapBase.x-w,trapBase.y),
          p1
        )
      val trapProg = Program(50.v, trapezoidFn,
        (_,pts) =>
          val p = Path(pts.map(p => p.map(_.primal)))
          p.attr("stroke"->"black","stroke-dasharray"->4,"fill"->"transparent")
          Seq(p),
        (_,pts,paths) => paths(0).update(pts.map(p => p.map(_.primal)))
      )

      val m1 = Mirror(trapProg, Axis(Pt(trapBase.x-125,0),Pt(trapBase.x-75,50)))
      val m2 = Mirror(m1, Axis(Pt(0,trapBase.y+100),Pt(250,trapBase.y+100)))
      val m3 = Mirror(m2, Axis(Pt(trapBase.x+99,0),Pt(trapBase.x+101,1000)))
      m3()

      def square(base : Pt[Double], r: Diff) : Seq[Pt[Diff]] =
        Seq(
          Pt[Diff](base.x,  base.y),
          Pt[Diff](base.x+r,base.y),
          Pt[Diff](base.x+r,base.y+r),
          Pt[Diff](base.x,  base.y+r))

      val r2 = (150.v)
      val base2 = Pt[Double](100,300)

      // arity 1
      val execute = (r: Diff) => square(base2, r)
      Program(r2, execute,
        (_,geo) => { val p = Path(geo.concretePoints); p.attr("fill","#228B22"); Seq(p) },
        (_,geo,e) => { val Seq(p) = e; p.update(geo.concretePoints) }
      )

      // Great, this works. Let's try a circle?

      val base3 = Pt[Double](500, 450)
      val (r,t) = (100.v, 0.v)

      val circleFn = (r: Diff, t: Diff) =>
        val iters = 12
        Seq.from((0 until iters).map(i =>
          val p = i.toDouble / iters
          val theta = (t/360.0) + p * (2*math.Pi)
          Pt[Diff](base3.x + r * theta.cos, base3.y + r * theta.sin)
        ))

      val p1 = Program((r,t), circleFn.tupled,
        { case ((r,_),_) => Seq(Circle(base3,s"${r.toInt}px",g)) },
        { case ((r,_),_,s) => s(0).attr("r",r.primal.toInt) }
      )

      val p2 = Program((r,t), circleFn.tupled,
        (_,geo) => (geo.concretePoints).map(p => Circle(p,"10px",g)) ,
        (_,geo,refs) => (geo.concretePoints).zip(refs).map((p,e) => e.setPosition(p))
      )

      // Now we need a reasonable abstraction for composing shapes, i.e. a multi-path one
      // that allows re-use of parameters and generation of paths through operations such
      // as `Mirror`, `Translate`, `Rotate`, and so on.

      // This is an attempt at such:
      val axis : Axis[Diff] = Axis(Pt(0,250),Pt(100,250))
      // val p3 = Mirror(p2, axis)()

      //
      val chairBase = Pt[Double](100,300)

      val chairFn = (theta : Diff, legHeight: Diff, width: Diff, length: Diff, legThick: Diff, backHeight: Diff) =>
        var rTheta = 2.0*math.Pi*(theta / 360.0)
        val cost = rTheta.cos; val sint = rTheta.sin
        def up(pt: Pt[Diff], len: Diff)   = Pt(pt.x, pt.y-len)
        def down(pt: Pt[Diff], len: Diff) = Pt(pt.x, pt.y+len)
        def fwd(pt: Pt[Diff], len: Diff)  = Pt(pt.x+len*cost, pt.y+len*sint)
        def iwd(pt: Pt[Diff], len: Diff)  = Pt(pt.x+len*cost, pt.y-len*sint)

        val b1 = Pt[Diff](chairBase.x,chairBase.y)
        val b2 = iwd(b1, width)
        val o1 = fwd(b1, length)
        val o2 = fwd(b2, length)

        val ls = Seq(b1,b2,o1,o2).map(p => down(p,legHeight))
        val u1 = up(b1, backHeight)
        val u2 = up(b2, backHeight)
        Seq(b2,o1,o2,u1,u2) ++ ls

      val chairArgs = (30.v, 100.v, 100.v, 100.v, 10.v, 100.v)
      // TODO: indices is a horrible way to do this
      val indices = Seq((0,1),(1,3),(3,2),(2,0),(0,4),(1,5),(4,5),(0,6),(1,7),(2,8),(3,9))
      val chairProg = Program(chairArgs, chairFn.tupled,
        (_,_p) => {
          val pts = Seq(chairBase) ++ _p.concretePoints
          val result = indices.map(is => Path(Seq(pts(is(0)),pts(is(1)))))
          result.foreach(s => s.attr("stroke" -> "black", "stroke-dasharray" -> 4))
          result
        },
        (_,_p,geos) => {
           val pts = Seq(chairBase) ++ _p.concretePoints
           val result = indices.map(is => Seq(pts(is(0)),pts(is(1))))
           result.zip(geos).map((ps,g) => g.update(ps))
        }
      )()

      val beamBase = Pt[Double](400,250)
      val beamArgs = (100.v, 100.v, 100.v, 100.v, 30.v)
      val beamFn = (l1 : Diff, l2: Diff, l3: Diff, l4: Diff, thk: Diff) =>
        val x0 = beamBase.x
        val x1 = x0 + l1
        val x2 = x1 + l2
        val x3 = x2 + l3
        val x4 = x3 + l4
        val xs = Seq[Diff](x0,x1,x2,x3,x4)
        val y0 = beamBase.y - thk
        xs.map(x => Pt[Diff](x,beamBase.y)) ++ xs.map(x => Pt(x,y0))

      val p2seq = (p : Seq[Pt[Double]]) =>
        val fwd = p.slice(0,5).toSeq
        val rev = p.slice(5,10).toSeq
        val p1 = fwd ++ (rev.reverse)
        val ps = fwd.zip(rev).map((p1,p2) => Seq(p1,p2))
        Seq(p1) ++ ps

      val beamProg = Program(beamArgs, beamFn.tupled,
        (p,g) =>
          val paths = p2seq(g.concretePoints).map(ps => Path(ps))
          paths.foreach(s => s.attr("stroke" -> "black", "stroke-dasharray" -> 4, "fill" -> "transparent"))
          paths,
        (p, g, paths) =>
          p2seq(g.concretePoints).zip(paths).map((points,path) => path.update(points))
      )()
    }

    val dragLayer = VectorLayer {
      val ctx = summon[SVGContext]
      val dwg = ctx.current.dwg

      val NUM_POINTS = 100
      val POINT_SCALE = 1000


      val pts = (0 to NUM_POINTS).map(_ =>
        val pt = Pt(math.random * POINT_SCALE, math.random * POINT_SCALE)
        val c = Circle(pt,"5px","transparent")
        c.attr("stroke","black")
        pt -> c
      ).toMap

      var fixedPts = Set[Pt[Double]]()

      def avg(set: Iterable[Pt[Double]]) =
        var xTotal = 0.0
        var yTotal = 0.0
        for (pt <- set)
          xTotal += pt.x
          yTotal += pt.y
        Pt(xTotal/set.size, yTotal/set.size)


      var gumball : Gumball = null;
      val ss = Selector(pts.keys.toSeq,
        ptSet =>
          for(p <- ptSet) pts(p).attr("fill","red")
          gumball.show(avg(ptSet))
        ,
        ptSet => ptSet.foreach(p =>
          if (!(fixedPts contains p))
            pts(p).attr("fill","transparent")
        )
      )

      gumball = Gumball(Pt(0,0), diff =>
        for (oldPt <- ss.selected if !(fixedPts contains oldPt))
          val oldCirc = pts(oldPt)
          oldPt.set(oldPt + diff)
          oldCirc.setPosition(oldPt)
      )

      document.addEventListener("keydown",(e : KeyboardEvent) =>
        def updateFixed =
          ss.clear
          fixedPts.foreach(p => pts(p).attr("fill","green"))
        if (e.key == "f")
          fixedPts ++= ss.selected
          updateFixed
        if (e.key == "g")
          fixedPts --= ss.selected
          updateFixed
      )
    }

    def randColor =
      val r = (math.random * 255).toInt
      val g = (math.random * 255).toInt
      val b = (math.random * 255).toInt
      s"rgb($r,$g,$b)"

    val colorLayer = VectorLayer {
      val SIZE = 200
      val BASE = Pt[Double](300,300)

      val pts = (0 to 2).map(i => (0 to 2).map(j => Pt[Double](i*SIZE,j*SIZE))).flatten.map(p => p + BASE)

      val shapes = pts.map(p => Circle(p,s"${SIZE/2}px",randColor))
      val blur = Blur("blur1",SIZE/2)
      val clip = Clip("clip1", svg("circle").attr("r"->SIZE,"cx"->(SIZE+BASE.x),"cy"->(SIZE+BASE.y)))

      for (shape <- shapes)
        shape.attr("filter",s"url(#${blur.name})")
        shape.attr("clip-path",s"url(#${clip.name})")

      val clipControl = Circle(Pt(SIZE+BASE.x,SIZE+BASE.y),s"${SIZE}px","transparent").draggable(newPt =>
        clip.element.attr("cx"->newPt.x,"cy"->newPt.y)
      )

      val controls = pts.zipWithIndex.map((p,i) => Circle(p,"5px","transparent").draggable(newPt =>
        shapes(i).setPosition(newPt)
      ).attr("stroke","black"))





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


