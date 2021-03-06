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
      (e: Event) =>
        nlopt.ready.`then`((_) =>
          // TestLagrangeMultipliers.run
          render
        )
    )

  def render =
    given Document = document
    given SVGContext = new SVGContext()

    document.body.withClass("center")

    var w = dom.window.innerWidth
    var h = dom.window.innerHeight

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
      val dMax = Math.max(ds.reduce(math.max),10.0)
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
          val viz = AmbiguityViz[Path](g,_ => p2seq(g))
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
            AmbiguityViz[Path](pts, _ => g2seq(geo)),
          (_,_,viz,diffs) =>
            val dt = normalizeOpacities(diffs)
            viz.update(dt)
      )()

    }

    val twoBoxes = () => VectorLayer {
      given ctx: DiffContext = new DiffContext()

      def boxFn(params: Tuple5[Diff,Diff,Diff,Diff,Diff], eq: EqualityContext[Diff]) =
        val (bx,by,r1,r2,t) = params
        val pts = Seq(
          Seq(
            Pt[Diff](0.0,0.0),
            Pt[Diff](r1,0.0),
            Pt[Diff](r1,r1),
            Pt[Diff](0.0,r1)
          ).map(p => p + Pt(bx,by)),
          Seq(
            Pt[Diff](0.0,t),
            Pt[Diff](r2,t),
            Pt[Diff](r2,t-r2),
            Pt[Diff](0.0,t-r2)
          ).map(p => p + Pt(bx,by))
        )
        eq.fix(pts(0)(2).y + 2, pts(1)(2).y)
        pts

      val boxArgs = (100.v,100.v,20.v,20.v,60.v)
      val boxProg = EqProgram(boxArgs,boxFn,
        (_,geo) => geo.map(s => Path(s.concretePoints)),
        (_,geo,paths,_) =>
          geo.zip(paths).map((g,p) => p.update(g.concretePoints))
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
              AmbiguityViz[Path](pts, _ => geo),
            (_,_,viz,diffs) =>
              val dt = normalizeOpacities(diffs)
              viz.update(dt)
      )().useParamLoss
    }

    val crvLayer = () => VectorLayer {
      given ctx: DiffContext = new DiffContext()

      val basePt = Pt(300.0,400.0)


      val crvFn = (
        h1: Diff, h2: Diff, h3: Diff, h4: Diff,
        w1: Diff, w2: Diff, w3: Diff, w4: Diff) =>
        val i = 5

        val dp = Pt[Diff](basePt.x, basePt.y)

        val v1 = dp + Pt(w1*0.5,0.0)
        val v2 = v1 - Pt(w2,h1)
        val v3 = v2 + Pt(w3,-h2)
        val v4 = v3 - Pt(w4,h3)
        val v5 = v4 + Pt(w1,-h4)

        val v10 = dp - Pt(w4*0.5,0.0)
        val v9 = v10 + Pt(w3,-h1)
        val v8 = v9 - Pt(w2,h2)
        val v7 = v8 + Pt(w1,-h3)
        val v6 = v7 - Pt(w4,h4)

        Seq(v1,v1,v2,v3,v4,
            v5,v5,v5,
            v6,v6,v6, // 10
            v7,v8,v9,v10,v10,v1)

      val crvArgs = (50.v,50.v,50.v,50.v, 50.v,50.v,50.v,50.v)
      val crvProg = Program2(crvArgs,crvFn.tupled,
        (_,g) =>
          def x(a: Int, b: Int) = Pt(g(a).x,g(b).y)
          def y(a: Int, b: Int) = Pt(g(b).x,g(a).y)

          val lbls = Seq(
            LineParameter("h1", crvArgs._1, g(0), x(0,2)),
            LineParameter("h2", crvArgs._2, g(2), x(2,3)),
            LineParameter("h3", crvArgs._3, g(3), x(3,4)),
            LineParameter("h4", crvArgs._4, g(4), x(4,5)),
            LineParameter("w1", crvArgs._5, g(15), y(15,16)),
            LineParameter("w2", crvArgs._6, g(13), y(13,2)),
            LineParameter("w3", crvArgs._7, g(12), y(12,3)),
            LineParameter("w4", crvArgs._8, g(11), y(11,4))
          )
          val viz = AmbiguityViz[Curve](g, _ => Seq(g))
          (lbls, viz),
        (_,_,r,diffs) =>
          val (lbls, viz) = r
          val dt = normalizeOpacities(diffs)
          viz.update(dt)
          lbls.map(_.update)
      )()
    }

    // TODO: move the layer utilities elsewhere and allow us just to specify the layer sequence here.

    // When the canvas is resized we need to tweak.
    def setLayerSize(layer: Layer): Layer =
      layer
        .attr("width", w.toInt)
        .attr("height", h.toInt)

    def drawLayer(layer: Layer): Layer =
      layer.draw(w, h)

    val options = Map[String,() => Layer](
      "Box"     -> twoBoxes,
      "Chair"   -> chairLayer,
      "Beam"    -> beamLayer,
      "Rotator" -> rotatorLayer,
      "Layout"  -> castleLayer,
      "Curve"   -> crvLayer,
    )

    val rootMenu = div("select")
    Seq("Box",
        "Chair",
        "Beam",
        "Curve",
        "Rotator",
        "Layout"
    ).map(o =>
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

    val p = (c: String) =>
      div("p").withHTML(c)

    setLayer("Box",twoBoxes())

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
