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

      val basePt = Pt(100.0,100.0)
      val crvFn = (h: Diff, w: Diff) =>
        val dp = Pt[Diff](basePt.x, basePt.y)
        Seq(
          dp,
          dp + Pt(w*0.5,h*0.1),
          dp + Pt(w,h),
          dp + Pt(w*1.5,-h*0.5)
        )
      val crvArgs = (100.v, 100.v)

      val crvProg = Program2(crvArgs,crvFn.tupled,
        (_,geo) =>
          AmbiguityViz[Curve](geo, _ => Seq(geo)),
        (_,_,viz,diffs) =>
          val dt = normalizeOpacities(diffs)
          viz.update(dt)
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
      "Chair" -> chairLayer,
      "Beam" -> beamLayer,
      "Rotator" -> rotatorLayer,
      "Layout" -> castleLayer,
      "Curve" -> crvLayer,
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

    val p = (c: String) =>
      div("p").withHTML(c)

    val text = div("div").child(
      p("Use the dropdown to control which program is being manipulated."),
      p("Try clicking and dragging on the points. When dragged, the inner part of the shape will highlight orange if the point is ambiguous, i.e our system thinks there are multiple viable places to place this point. Note that some demos contain an anchor point, and as a result some points may not move."),
      p("Each demo controls an individual program. When a point is dragged, the program is automatically differentiated and a numerical optimizer is run to determine new program parameters. The program is then re-executed to update the SVG shape. Parameter values are displayed as labels on the dotted lines. As a result the ambiguity values continually update, and their values in the middle of a manipulation also contain information."),
      p("Our visualization is designed to: <ul>" +
        "<li>Work for arbitrary differentiable programs</li>" +
        "<li>Project into the shape's context</li>" +
        "<li>Update interactively as manipulated (since the data being visualized in some sense <em>is</em> the manipulation in the context of the program</li>"+
        "</ul>"),
      p(""),
      p("Ambiguity is calculated by taking the squared distance between the point locations generated by applying the optimization with different loss function terms in conjunction with the primary loss that minimizes the distance to the edited vertex. In our case, we compare between <em>Vertex Loss</em> and <em>Parameter Loss</em>, where vertex loss attempts to satisfy the edit while minimizing the change in all other vertex positions, and parameter loss satisfies the edit while minimizing the L2 distance between the original and final parameter vectors. Parameters are optimized using the start point of the current mouse stroke as the initial vector (i.e. the location of the last mousedown), denoted by a dotted blue line while dragging."),
    ).withClass("explain")
    document.body.appendChild(text)

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
