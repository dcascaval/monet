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

    def avg(set: Iterable[Pt[Double]]) =
      var xTotal = 0.0
      var yTotal = 0.0
      for (pt <- set)
        xTotal += pt.x
        yTotal += pt.y
      Pt(xTotal/set.size, yTotal/set.size)

    def randColor =
      val r = (math.random * 255).toInt
      val g = (math.random * 255).toInt
      val b = (math.random * 255).toInt
      s"rgb($r,$g,$b)"

    val normalizeOpacities = (ptPositions: Seq[Seq[Pt[Double]]]) =>
      val Seq(d1,d2) = ptPositions
      val ds = d1.zip(d2).map((a,b) => a.dist(b))
      val dMin = ds.reduce(math.min)
      val dMax = Math.max(ds.reduce(math.max),1.0)
      ds.map(d => (d-dMin)/dMax).map(t => t*t)


    val chairLayer = () => VectorLayer {

      given ctx: DiffContext = new DiffContext()
      val blur = Blur("blur1", 20)
      val chairBase = Pt[Double](200,400)

      val chairFn = (legHeight: Diff, width: Diff, length: Diff, legThick: Diff, backHeight: Diff) =>
        var rTheta = 2.0*math.Pi*(30.0 / 360.0)
        val cost = rTheta.cos; val sint = rTheta.sin
        def up(pt: Pt[Diff], len: Diff)   = Pt(pt.x, pt.y-len)
        def down(pt: Pt[Diff], len: Diff) = Pt(pt.x, pt.y+len)
        def fwd(pt: Pt[Diff], len: Diff)  = Pt(pt.x+len*cost, pt.y+len*sint)
        def iwd(pt: Pt[Diff], len: Diff)  = Pt(pt.x+len*cost, pt.y-len*sint)

        val b1 = up(Pt[Diff](chairBase.x,chairBase.y),legHeight)
        val b2 = iwd(b1, width)
        val o1 = fwd(b1, length)
        val o2 = fwd(b2, length)

        val ls = Seq(b1,b2,o1,o2).map(p => down(p,legHeight))
        val u1 = up(b1, backHeight)
        val u2 = up(b2, backHeight)
        Seq(b1,b2,o1,o2,u1,u2) ++ ls

      val chairArgs = (100.v, 100.v, 100.v, 10.v, 100.v)
      // TODO: indices is a horrible way to do this
      val indices = Seq((0,1),(1,3),(3,2),(2,0),(0,4),(1,5),(4,5),(0,6),(1,7),(2,8),(3,9))
      val fillIndices = Seq(Seq(0,1,3,2),Seq(0,1,5,4))

      val chairProg = Program2(chairArgs, chairFn.tupled,
        (_,_p) => {
          val pts = _p.concretePoints
          val result = indices.map(is =>
            Path(Seq(pts(is(0)),pts(is(1))))
              .attr("stroke" -> "black", "stroke-dasharray" -> 4)
          )
          val clip = Clip("clip1",fillIndices.map(is =>
            Path(is.map(i => pts(i)))
          ))
          val circs = pts.map(p =>
            Circle(p,20,"#FFA500")
              .blur(blur).clip(clip)
              .attr("opacity",0)
          )
          val lbls = Seq(
            LineParameter("h",chairArgs._1,_p(0),_p(6)),
            LineParameter("w",chairArgs._2,_p(5),_p(4))
          )

          (result, clip, circs, lbls)
        },
        (_,_p,elements, ptPositions) => {
           val Seq(d1,d2) = ptPositions
           val ds = d1.zip(d2).map((a,b) => a.dist(b))
           val dMin = ds.reduce(math.min)
           val dMax = Math.max(ds.reduce(math.max),1.0)
           val dt = ds.map(d => (d-dMin)/dMax)

           val (geos, clip, circs, lbls) = elements
           val pts = _p.concretePoints
           val cen = avg(pts)
           val result = indices.map(is => Seq(pts(is(0)),pts(is(1))))
           val fills = fillIndices.map(is => is.map(i => pts(i)))
           lbls.map(_.update)

           circs.zip(pts).map((c,p) => c.setPosition(p))

           if (dMin != dMax)
            circs.zip(dt).map((c,t) =>
                c.attr("opacity",t)
            )
           clip.content.zip(fills).map((g,ps) => g.update(ps))
           geos.zip(result).map((g,ps) => g.update(ps))
        }
      )()
    }

    val beamLayer = () => VectorLayer {
      given ctx: DiffContext = new DiffContext()

      val beamBase = Pt[Double](200,250)
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

      def p2seq[T](p : Seq[Pt[T]]) =
        val fwd = p.slice(0,5).toSeq
        val rev = p.slice(5,10).toSeq
        val p1 = fwd ++ (rev.reverse)
        val ps = fwd.zip(rev).map((p1,p2) => Seq(p1,p2))
        Seq(p1) ++ ps

      val beamProg = Program2(beamArgs, beamFn.tupled,
        (p,g) =>
          val viz = AmbiguityViz(g,_ => p2seq(g))
          val lbls = Seq(
            LineParameter("x1", beamArgs._1, g(0), g(1)),
            LineParameter("x2", beamArgs._2, g(1), g(2)),
            LineParameter("x3", beamArgs._3, g(2), g(3)),
            LineParameter("x4", beamArgs._4, g(3), g(4)),
            LineParameter("y",  beamArgs._5, g(5), g(0))
          )
          (viz,lbls),
         (_, _, result, ptPositions) =>
          val (viz,lbls) = result
          val dt = normalizeOpacities(ptPositions)
          viz.update(dt)
          lbls.map(_.update)
      )()

      // val triBase = Pt[Double](100,800)
      // val triArgs = (50.v,50.v)
      // val (triH, triV) = (6,6)
      // val triFn = (w: Diff, h: Diff) =>
      //   Seq[Pt[Diff]](
      //     Pt(triBase.x, triBase.y),
      //     Pt(triBase.x+(w/2),triBase.y-h),
      //     Pt(triBase.x+triH*w+w/2,triBase.y-(triV*h))
      //   )
      // val triProg = Program2(triArgs, triFn.tupled,
      //   (p,g) =>
      //     val (w,h) = (p._1.toInt, p._2.toInt)
      //     (0 until triV).flatMap(i =>
      //       (0 until triH).map(j =>
      //           val tx = triBase.x + j * w
      //           val ty = triBase.y - (i * h)
      //           (Path(Seq(
      //             Pt(tx,ty),
      //             Pt(tx+(w/2),ty-h),
      //             Pt(tx+w,ty)
      //           )).attr("fill" -> "transparent")
      //             .attr("stroke" -> "black")
      //             .close
      //           ,
      //           Path(Seq(
      //             Pt(tx+(w/2),ty-h),
      //             Pt(tx+w,ty),
      //             Pt(tx+w+(w/2),ty-h)
      //           )).attr("fill" -> "transparent")
      //             .attr("stroke" -> "black")
      //             .close
      //     )))
      //   ,
      //   (p,g,q,ptPositions) =>
      //     val (w,h) = (p._1.toInt, p._2.toInt)
      //     val seqs = (0 until triV).flatMap(i =>
      //       (0 until triH).map(j =>
      //           val tx = triBase.x + j * w
      //           val ty = triBase.y - (i * h)
      //           (Seq(
      //             Pt(tx,ty),
      //             Pt(tx+(w/2),ty-h),
      //             Pt(tx+w,ty)
      //           ),
      //           Seq(
      //             Pt(tx+(w/2),ty-h),
      //             Pt(tx+w,ty),
      //             Pt(tx+w+(w/2),ty-h)
      //           ))
      //     ))
      //     seqs.zip(q).map { case ((s1,s2),(p1,p2)) =>
      //       p1.update(s1)
      //       p2.update(s2)
      //     }
      // )().useParamLoss
    }

    val rotatorLayer = () => VectorLayer {
      given ctx: DiffContext = new DiffContext()

      val numBlocks = 8
      val rCen = Pt[Double](300,300)
      val radialFn = (r: Diff, t: Diff, l: Diff) =>
        val diff = Pt(l,t)
        Seq.from((0 until numBlocks).map(i =>
          val theta = (i / numBlocks.toDouble) * 2.0 * math.Pi
          val ctr = Pt(rCen.x + r*(theta.cos), rCen.y + r*(theta.sin))
          val offs = diff.rotate(theta)
          (ctr - offs, ctr + offs)))

      val g2seq = (geo: Seq[(Pt[Diff],Pt[Diff])]) =>
        geo.map((l,r) => Seq(
                  Pt(l.x,l.y),
                  Pt(r.x,l.y),
                  Pt(r.x,r.y),
                  Pt(l.x,r.y)
              ))

      val radArgs = (100.v, 10.c, 20.c)
      val radProg = Program2(radArgs, radialFn.tupled,
          (_,geo) =>
            val pts = geo.flatMap((a,b) => Seq(a,b))
            AmbiguityViz(pts, _ => g2seq(geo)),
          (_,_,viz,diffs) =>
            val dt = normalizeOpacities(diffs)
            viz.update(dt)
      )()

    }

    val castleLayer = () => VectorLayer {
      given ctx: DiffContext = new DiffContext()
      val casteFn = (
        b1 : Diff, b2: Diff,
        l1: Diff, l2: Diff, l3: Diff, thk1: Diff, thk2: Diff,
        towerRbig: Diff, towerRSmall: Diff,
        extend: Diff
      ) =>
        val rr = (t1 : Pt[Diff], t2: Pt[Diff]) =>
          Seq(t1,Pt(t1.x,t2.y),t2,Pt(t2.x,t1.y))

        val cx = b1
        // Lower tower
        val t1 = rr(
          Pt(cx - towerRbig, b2 + towerRbig),
          Pt(cx + towerRbig, b2 - towerRbig))

        val w1y = b2 - towerRbig
        val t2y = w1y - l1

        val w1 = rr(
          Pt(cx - thk1, w1y),
          Pt(cx + thk1, t2y)
        )

        // Middle Tower
        val w2y = t2y - 2*towerRSmall
        val t2 = rr(
          Pt(cx-towerRSmall,t2y),
          Pt(cx+towerRSmall,w2y))

        val w2 = rr(
          Pt(cx - thk1, w2y),
          Pt(cx + thk1, w2y - l2))

        val t3y = (t2y-towerRSmall)
        val t3x = cx+towerRSmall+l3
        // Offshoot tower
        val w3 = rr(
          Pt(cx+towerRSmall,t3y-thk2),
          Pt(t3x,t3y+thk2)
        )
        val t3 = rr(
          Pt(t3x,t3y-towerRSmall),
          Pt(t3x+2*towerRSmall,t3y+towerRSmall)
        )

        // Pivoted tower
        val root2 = math.sqrt(2)
        val t4r2 = 2*towerRbig / root2
        val mid4x = (w2(1).x+w2(2).x) * 0.5
        val r4 = Pt(mid4x, w2(2).y - (mid4x - w2(2).x))
        val r1 = Pt(mid4x, r4.y - (2*t4r2))
        val r2 = Pt(mid4x + t4r2, r4.y - t4r2)
        val r3 = Pt(mid4x - t4r2, r4.y - t4r2)
        val t4 = Seq(r1,r2,r4,r3)

        val theta = 45.0 * 0.01754329
        val rotRect = rr(
          Pt(-thk1,0.0),
          Pt(thk1,extend)
        )
        val rotcpt = Pt(mid4x, r2.y)
        val rotRect1 = rotRect.map(_.rotate(theta)).map(p => p+rotcpt)
        val rotRect2 = rotRect.map(_.rotate(-theta)).map(p => p+rotcpt)


        val tsr2 = towerRSmall / root2
        val rotTower = rr(Pt(-towerRSmall,-towerRSmall),Pt(towerRSmall,towerRSmall))
        val rotFix1 = (rotRect1(1)+rotRect1(2))*0.5 + Pt(-tsr2,-tsr2)
        val rotFix2 = (rotRect2(1)+rotRect2(2))*0.5 + Pt( tsr2,-tsr2)
        val rotTower1 = rotTower.map(_.rotate(theta)).map(_+rotFix1)
        val rotTower2 = rotTower.map(_.rotate(-theta)).map(_+rotFix2)

        Seq(t1,w1,t2,w2,w3,t3,t4,rotRect1,rotRect2,rotTower1,rotTower2)

      val casteArgs = (
        300.v,500.v,
        40.v, 30.v, 60.v, 20.v, 20.v,
        40.v, 30.v,
        80.v
      )

      val casteProg = Program2(casteArgs, casteFn.tupled,
            (_,geo) =>
              val pts = geo.flatten
              AmbiguityViz(pts, _ => geo),
            (_,_,viz,diffs) =>
              val dt = normalizeOpacities(diffs)
              viz.update(dt)
      )().useParamLoss
    }

    val dragLayer = () => VectorLayer {
      val ctx = summon[SVGContext]
      val dwg = ctx.current.dwg

      val NUM_POINTS = 100
      val POINT_SCALE = 1000


      val pts = (0 to NUM_POINTS).map(_ =>
        val pt = Pt(math.random * POINT_SCALE, math.random * POINT_SCALE)
        val c = Circle(pt,"5px","transparent").attr("stroke","black")
        pt -> c
      ).toMap

      var fixedPts = Set[Pt[Double]]()

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


    val colorLayer = () => VectorLayer {
      val SIZE = 200
      val BASE = Pt[Double](300,300)

      val pts = (0 to 2).map(i => (0 to 2).map(j => Pt[Double](i*SIZE,j*SIZE))).flatten.map(p => p + BASE)

      val shapes = pts.map(p => Circle(p,SIZE/2,randColor))
      val blur = Blur("blur1",SIZE/2)

      val clip = Clip("clip1",
        Circle(Pt(SIZE+BASE.x,SIZE+BASE.y), SIZE)
      )

      val clipControl = Circle(Pt(SIZE+BASE.x,SIZE+BASE.y), SIZE)
        .attr("fill","transparent")
        .draggable(newPt => clip.content.setPosition(newPt))

      for (shape <- shapes)
        shape.attr("filter",s"url(#${blur.name})")
        shape.attr("clip-path",s"url(#${clip.name})")

      val controls = pts.zipWithIndex.map((p,i) => Circle(p,5).draggable(newPt =>
        shapes(i).setPosition(newPt)
      ).attr("stroke","black"))



    }

    val layers : Seq[Layer] = Seq()

    // TODO: move the layer utilities elsewhere and allow us just to specify the layer sequence here.

    // When the canvas is resized we need to tweak.
    def setLayerSize(layer: Layer): Layer =
      layer
        .attr("width", w.toInt)
        .attr("height", h.toInt)

    def drawLayer(layer: Layer): Layer =
      layer.draw(w, h)

    val options = Map[String,() => Layer](
      "Chair" -> chairLayer,
      "Beam" -> beamLayer,
      "Rotator" -> rotatorLayer,
      "Layout" -> castleLayer
    )

    val rootMenu = div("select")
    options.map((o,_) =>
      val d = div("option")
      d.innerHTML = o
      d.attr("value", o)
      rootMenu.appendChild(d))

    document.body.appendChild(
      rootMenu
    )

    var currentLayerName : String = ""
    var currentLayer : Layer = null

    var resetButton = div("button")
    resetButton.innerHTML = "Reset"
    document.body.appendChild(resetButton)

    resetButton.onclick = e =>
      currentLayer.clear
      setLayer(currentLayerName, options(currentLayerName)())


    def setLayer(name: String, layer: Layer) =
      if (currentLayer != layer)
        currentLayerName = name
        currentLayer = layer
        setLayerSize(currentLayer)
        drawLayer(currentLayer)
        document.body.insertBefore(currentLayer.element, rootMenu)

    setLayer("Chair",chairLayer())

    rootMenu.onchange = e =>
      val newLayerName = rootMenu.options(rootMenu.selectedIndex).value
      val newLayer = options(newLayerName)()
      currentLayer.clear
      setLayer(newLayerName, newLayer)




    dom.window.addEventListener(
    "resize",
    (_: Event) =>
      w = dom.window.innerWidth
      h = dom.window.innerHeight
      setLayerSize(currentLayer)
      drawLayer(currentLayer)
  )
