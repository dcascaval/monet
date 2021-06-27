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
import org.scalajs.dom.svg
import org.scalajs.dom.raw.Document
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.compiletime.ops.string
import scala.annotation.meta.param
import org.scalajs.dom.raw.HTMLOptionElement
import org.scalajs.dom.raw.HTMLSelectElement
import org.scalajs.dom.raw.HTMLButtonElement
import org.scalajs.dom.raw.HTMLParagraphElement
import org.scalajs.dom.raw.HTMLDivElement

class Pt[T](var x : T, var y : T)(using Operations[T]):
  import GenericTSyntax._
  def +(other: Pt[T]) =
    Pt(x + other.x, y + other.y)

  def -(other: Pt[T]) =
    Pt(x - other.x, y - other.y)

  def *(other: Double) =
    Pt(x * other, y * other)

  def dist(other: Pt[T]) =
    val dx = other.x - x
    val dy = other.y - y
    (dx*dx) + (dy*dy)

  def toSVG =
    s"${x.toInt} ${y.toInt}"

  def set(other: Pt[T]): Unit =
    x = other.x
    y = other.y

  def map[Q : Operations](f : T => Q): Pt[Q] =
    Pt(f(x),f(y))

  def rotate(theta: T) : Pt[T] =
    val sint = theta.sin
    val cost = theta.cos
    Pt(x*cost - y*sint, - (x*sint + y*cost))

  def duplicate: Pt[T] = Pt(x,y)

def rescaleVector(pt: Pt[Double], length: Double) =
  val ratio = length / math.sqrt(pt.x * pt.x + pt.y * pt.y)
  pt * ratio

object Pt:
  def unapply[T](pt: Pt[T]) : (T,T) = (pt.x,pt.y)

trait Domable[A]:
  def element(a: A): Element

given [T <: Element] : Domable[T] with
  def element(e: T): T = e

given Domable[Circle] with
  def element(p: Circle): Element = p.circ

given Domable[Rectangle] with
  def element(p: Rectangle): Element = p.path

given Domable[Curve] with
  def element(p: Curve): Element = p.path


// Extensions to get a builder-pattern style syntax for SVG attributes,
// Useful for things like styles, hierarchies, clipping, so on.
extension [T : Domable](element: T)
  def elt : Element = summon[Domable[T]].element(element)

  def withClass(cls: String) : T =
    elt.classList.add(cls)
    element

  def withHTML(inner: String) : T =
    elt.innerHTML = inner
    element

  def withStyle(styles: (String,Any)*) : T =
    val text = styles.map((k,v) => s"${k}:${v};").reduce(_+_)
    elt.setAttributeNS(null,"style",text)
    element

  def attr[Q](key: String, value: Q): T =
    elt.setAttributeNS(null, key, value.toString)
    element

  def attr(mapping: (String,Any)*): T =
    for ((k,v) <- mapping) elt.setAttributeNS(null,k,v.toString)
    element

  def child[Q <: Element](makeChild: => Q*): T =
    for (m <- makeChild) elt.appendChild(m)
    element

  def draw()(using ctx: SVGContext) : T =
    ctx.current.addElement(elt)
    element

  def define()(using ctx: SVGContext) : T =
    ctx.current.addDefinition(elt)
    element

  def blur(b: Blur) : T =
    elt.attr("filter", s"url(#${b.name})")
    element

  def clip[Q](c: Clip[Q]) : T =
    elt.attr("clip-path", s"url(#${c.name})")
    element

  def mask(m: Mask) : T =
    elt.attr("mask",s"url(#${m.id})")
    element

def updateCircleCenter[T : Operations](element: Element, pt: Pt[T]) =
  import GenericTSyntax._
  element
    .attr("cx",pt.x.toInt)
    .attr("cy",pt.y.toInt)

def svg(tag: String) =
  document.createElementNS(SVG.URI, tag)

type Tag = "option" | "select" | "button" | "div" | "p"

type ConstructElement[T <: Tag] = T match {
  case "option" => HTMLOptionElement
  case "select" => HTMLSelectElement
  case "button" => HTMLButtonElement
  case "div" => HTMLDivElement
  case "p" => HTMLParagraphElement
  case _ => Element
}

def div[T<:Tag](tag: T) : ConstructElement[T] =
  document.createElement(tag).asInstanceOf[ConstructElement[T]]

object SVG:
  val URI = "http://www.w3.org/2000/svg"
  def apply(): SVG =
    new SVG(using document)
  def apply(root: Element) : SVG =
    val result = new SVG(using document)
    result.dwg = root.asInstanceOf[SVGElement]
    result

class SVG()(using doc: Document):
  var dwg = svg("svg").asInstanceOf[SVGElement]
  val def_element = svg("defs")
  dwg.appendChild(def_element)
  val elts = ArrayBuffer[Element]()

  def apply(): SVGElement = dwg
  def addDefinition(element: Element): Unit =
    def_element.appendChild(element)

  def addElement(element: Element): Unit =
    elts += element
    dwg.appendChild(element)


sealed trait Gradient:
  val name : String
  val element: Element

case class RadialGradient(name: String, stops: (Int, String)*)(using
    ctx: SVGContext
) extends Gradient { self =>
  private val sortStops = stops.toVector.sortBy { case (a, _) => a }

  val element = svg("radialGradient")
    .attr("id", name)
    .attr("gradientUnits", "userSpaceOnUse")
  for ((percent, color) <- sortStops)
    val stopElement = svg("stop")
      .attr("offset", percent)
      .attr("stop-color", color)
    element.appendChild(stopElement)
  ctx.current.addDefinition(element)
}

case class Blur(name: String, stdDev: Double)(using ctx: SVGContext):
  val filter = svg("filter").attr("id" -> name, "x" -> "-250%", "y" -> "-250%", "width" -> "650%", "height" -> "650%")
  val blur = svg("feGaussianBlur").attr("in" -> "SourceGraphic", "stdDeviation" -> stdDev)
  filter.appendChild(blur)
  filter.define()

case class Mask(id: String, artwork: Element => Any)(using ctx: SVGContext):
  val element = svg("mask")
    .attr("id", id)
    .child(
       svg("rect")
        .attr("fill" -> "white",
              "width" -> "100%",
              "height" -> "100%"))
  artwork(element)
  element.define()

class Clip[T](val name: String, element: SVGContext ?=> T)(using outerContext: SVGContext):
  val innerContext = new SVGContext()
  val root = svg("clipPath").attr("id",name)
  root.define()
  val newRoot = SVG(root)
  innerContext.push(newRoot)
  val content = element(using innerContext)
  for (e <- innerContext.current.elts)
    root.appendChild(e)


class Group[T](val name: String, element: SVGContext ?=> T)(using outerContext: SVGContext):
  val root = svg("g").attr("id",name)
  root.draw()

  val newRoot = SVG(root)
  val innerContext = new SVGContext()
  innerContext.push(newRoot)
  val content = element(using innerContext)
  for (e <- innerContext.current.elts)
    root.appendChild(e)


given Domable[Layer] with
  def element(a: Layer) = a.element

sealed trait Layer { self =>
  def element: Element
  def draw(w: Double, h: Double): Layer = self
  def clear =
    document.body.removeChild(element)
}

extension (canvas : HTMLCanvasElement)
  def context2D : CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

extension (e: MouseEvent)
  def shiftKey : Boolean =
    e.getModifierState("Shift")
  def ctrlKey : Boolean =
    e.getModifierState("Ctrl")

sealed class SVGContext:
  val layers: ArrayBuffer[SVG] = ArrayBuffer()
  def push(v: SVG) = layers.append(v)
  def current = layers.last

object Layers:
  def VectorLayer(artwork: => Any)(using ctx: SVGContext, doc: Document): Layer =
    val svg = SVG()
    svg()
      .withClass("vec-layer")
      .withStyle("opacity" -> 0.9)
    ctx.push(svg)
    artwork
    new Layer { val element = svg() }


  def PixelLayer(drawCallback: CanvasRenderingContext2D ?=> Unit)(using doc: Document): Layer =
    var w = dom.window.innerWidth
    var h = dom.window.innerHeight
    val canvas = doc.createElement("canvas").asInstanceOf[HTMLCanvasElement].withClass("pix-layer");
    given ctx : CanvasRenderingContext2D = canvas.context2D

    val layer = new Layer { self =>
      val element = canvas
      override def draw(w: Double, h: Double) : Layer =
        drawCallback(using ctx)
        self
    }
    layer.draw(w, h)
    layer



case class Circle(var position: Pt[Double], val radius: String | Int, val fill: String | Gradient = "transparent")
  (using ctx: SVGContext) { self =>
  val circ = svg("circle")
  updateCircleCenter(circ, position)
  // TODO: Don't rely on a .draw() call for this, automatically add any created elements to an SVG context
  circ
    .attr("r", radius)
    .attr("fill", fill match
      case style : String => style
      case grad : Gradient => s"url(#${grad.name})")
    .draw()
  def setPosition(p : Pt[Double]) =
    updateCircleCenter(circ, p)
    position = p
}

given Domable[Path] with
  def element(p: Path): Element = p.path

object Path:
  def empty(using SVGContext) : Path = Path(Seq())

case class Path(var points: Seq[Pt[Double]])(using ctx: SVGContext) { self =>
  val path = svg("path")

  var closed = false

  def update(newPoints: Seq[Pt[Double]]) =
    points = newPoints
    var pathString =  if (points.length > 0) s"M ${points(0).toSVG}" else ""
    var rest = if (points.length > 1) points.drop(1).map(p => s"L ${p.toSVG}").reduce(_+_) + (if (closed) " z" else "") else ""
    path
      .attr("d",pathString+rest)

  def makeClosed =
    closed = true
    update(points)
    self

  update(points)
  path.draw()
}

case class Curve(points: Seq[Pt[Double]])(using ctx: SVGContext) { self =>
  val path = svg("path")
  // We wanna make a spline through these points. This means figuring out
  // the control points and the handles. To approximate this we'll just
  // take each handle as the average of the vectors in each direction.

  def update(points: Seq[Pt[Double]]) =
    def getPtSaturating(i: Int) : Pt[Double] =
      val k = if (i < 0) 0 else if (i >= points.length) points.length -1 else i
      points(k)

    val handles = points.zipWithIndex.map((p,i) =>
      val prev = getPtSaturating(i-1)
      val next = getPtSaturating(i+1)
      val dPrev = p - prev
      val dNext = next - p
      val handle = (dPrev + dNext) * 0.25
      handle
    )

    val paths = (0 until (points.length-1)).map(i =>
      val p1 = points(i)
      val p2 = points(i+1)
      val h1 = p1 + handles(i)
      val h2 = p2 - handles(i+1)
      s"M ${p1.toSVG} C ${h1.toSVG}, ${h2.toSVG}, ${p2.toSVG}"
    ).mkString(" ")
    path.attr("d",paths)

  update(points)
  path.draw()
}


object Clip:
  var id = 0;
  def freshTemp =
    id += 1
    s"clip_$id"

object Blur:
  var id = 0;
  def freshTemp =
    id += 1
    s"blur_$id"

// See `Curve` for why the clip fns are necessary
trait StateElem[T]:
  def create(pts: Seq[Pt[Double]])(using SVGContext) : T
  def createClip(pts: Seq[Pt[Double]])(using SVGContext) : T
  def update(t: T, pts: Seq[Pt[Double]])(using SVGContext) : Unit
  def updateClip(t: T, pts: Seq[Pt[Double]])(using SVGContext) : Unit
  def close(t: T) : T

extension [T : StateElem](t: T)
  def close : T = summon[StateElem[T]].close(t)

given StateElem[Path] with
  def create(pts: Seq[Pt[Double]])(using SVGContext) : Path = Path(pts)
  def createClip(pts: Seq[Pt[Double]])(using SVGContext) : Path = Path(pts)
  def update(t : Path, pts: Seq[Pt[Double]])(using SVGContext) : Unit = t.update(pts)
  def updateClip(t : Path, pts: Seq[Pt[Double]])(using SVGContext) : Unit = t.update(pts)
  def close(t: Path) = t.makeClosed

given StateElem[Curve] with
  def create(pts: Seq[Pt[Double]])(using SVGContext) : Curve = Curve(pts)

  // HACK WARNING: EXTREME
  def createClip(pts: Seq[Pt[Double]])(using SVGContext) : Curve =
    // val w = dom.window.innerWidth
    // val h = dom.window.innerHeight
    // // imperative programming is the way. this creates itself
    // // in the active SVG context, which is the one in the clip.
    // Path(Seq(Pt(0,0),Pt(w,0),Pt(w,h),Pt(0,h),Pt(0,0)))
    // Return a dummy
    Curve(pts)

  def update(t : Curve, pts: Seq[Pt[Double]])(using SVGContext) : Unit = t.update(pts)

  // See above
  def updateClip(t: Curve, pts: Seq[Pt[Double]])(using SVGContext) : Unit = t.update(pts)
  def close(t: Curve) = t

case class AmbiguityViz[T : Domable](
    pts: Seq[Pt[Diff]],
    exec: Seq[Pt[Diff]] => Seq[Seq[Pt[Diff]]],
)(using SVGContext, StateElem[T]):
  private val pathOps = summon[StateElem[T]]

  var ipts = exec(pts)
  val clip = Clip(Clip.freshTemp, {
    ipts.map(r => pathOps.createClip(r.concretePoints))
  })

  val rects = ipts.map(r => pathOps.create(r.concretePoints)
      .attr("fill","transparent")
      .attr("stroke-dasharray", 4)
      .attr("stroke", "black")
      .close
  )

  val blur = Blur(Blur.freshTemp, 20)
  val circs = pts.concretePoints.map(p =>
    Circle(p, 50)
    .attr("fill","rgb(255,165,0)")
    .blur(blur).clip(clip)
    .attr("opacity",0))

  def update(differences: Seq[Double]) =
    ipts = exec(pts)
    circs.zip(pts.concretePoints).map((c,p) => c.setPosition(p))
    clip.content.zip(ipts).map((c,r) => pathOps.updateClip(c,r.concretePoints))
    rects.zip(ipts).map((r,p) => pathOps.update(r,p.concretePoints))
    circs.zip(differences).map((c,t) => c.attr("opacity", t))


case class Gumball(var center : Pt[Double], onChange: Pt[Double] => Unit)(using SVGContext):
  val SIZE = 100
  val WIDTH = 2

  var v = Rectangle(Pt(0,0),Pt(0,0))
    .attr("fill","blue")
    .attr("stroke-width","20")
    .attr("stroke","transparent")
  var h = Rectangle(Pt(0,0),Pt(0,0))
    .attr("fill","red")
    .attr("stroke-width","20")
    .attr("stroke","transparent")

  val elements = Seq[(Rectangle,Pt[Double]=>Pt[Double])](
    (v,diff => Pt(0,diff.y)),(h,diff => Pt(diff.x,0))
  )

  for ((e,mapDiff) <- elements) e.draggable(newPoint =>
    val diff = mapDiff(newPoint - center)
    for ((f,_) <- elements)
      f.update(center+diff,center+diff+(f.p2-f.p1))
    onChange(diff)
    center += diff
  )

  def show(newCenter: Pt[Double]) =
    center = newCenter
    v.update(center,Pt(center.x+WIDTH,center.y-SIZE))
    h.update(center,Pt(center.x+SIZE,center.y+WIDTH))

  def hide =
    v.update(Pt(0,0),Pt(0,0))
    h.update(Pt(0,0),Pt(0,0))

  v.draw()
  h.draw()

def order(a: Double, b: Double) =
  if (a<=b) (a,b) else (b,a)

case class Rectangle(var p1: Pt[Double], var p2: Pt[Double])(using ctx: SVGContext):
  val path = svg("rect")

  def update(nextP1: Pt[Double], nextP2: Pt[Double]) =
    p1 = nextP1; p2 = nextP2
    val (minx,maxx) = order(p1.x,p2.x)
    val (miny,maxy) = order(p1.y,p2.y)
    path
      .attr("x",minx.toInt)
      .attr("y",miny.toInt)
      .attr("width",(maxx-minx).toInt)
      .attr("height",(maxy-miny).toInt)

  update(p1,p2)
  path.draw()

class LineParameter(name: String, value: Diff, p1: Pt[Diff], p2: Pt[Diff])(using s: SVGContext, d: DiffContext):
  val path = svg("path").attr(
      "fill"->"transparent",
      "stroke"->"red",
      "stroke-dasharray"->5,
      "stroke-width"->0.5,
  )

  val txt = svg("text")
    .attr("text-anchor","middle")

  def update =
    txt.innerHTML = s"$name: ${value.toInt}"
    val (p1x,p1y,p2x,p2y) = (p1.x.toInt, p1.y.toInt , p2.x.toInt , p2.y.toInt)
    val Pt(ox,oy) = rescaleVector(Pt(p2y-p1y,p2x-p1x),10)
    val (x1,y1,x2,y2) = (p1x-ox,p1y+oy,p2x-ox,p2y+oy)
    txt
      .attr("x",((x2+x1)/2)-ox*2)
      .attr("y",((y2+y1)/2)+oy*2)
    path.attr("d",s"M $x1 $y1 L $x2 $y2")

  update
  path.draw()
  txt.draw()

case class Axis[T : Operations](a: Pt[T], b: Pt[T]):
  import GenericTSyntax._
  val diff = b.x - a.x
  val slope = (b.y - a.y) / diff
  val yInt = (b.x*a.y - a.x*b.y) / diff
  val s2 = (slope*slope) + 1.0

  def reflect(c: Pt[T]) =
    val d = ((c.x + (c.y - yInt)*slope) / s2) * 2.0
    Pt[T](d - c.x, d * slope - c.y + yInt*2)

// Each program takes as input a set of initial parameters that serve as the root
// of our optimization, a function to execute to find the positions of the control
// points, and a path function that redraws the path given a combination of parameters
// and control points.
//
// This avoids duplicating work in the case where the path _is_ the control points,
// and allows for rendering to multiple paths etc. It is up to the author of the
// path function what level of bidirectionality they want to expose.

trait PointObject[A]:
  def points(a: A): Seq[Pt[Diff]]
  def mirror(a: A, axis: Axis[Diff]): A
  def duplicate(a: A): A
  def join(a: A, other: A): A

extension [A](a: A)(using obj: PointObject[A])
  def points : Seq[Pt[Diff]] = obj.points(a)
  def concretePoints : Seq[Pt[Double]] = obj.points(a).map(_.map(_.primal))
  def diffElements : Seq[Diff] = obj.points(a).flatMap(pt => Seq(pt.x,pt.y))
  def mirror(axis: Axis[Diff]) = obj.mirror(a,axis)
  def duplicate : A = obj.duplicate(a)
  def join(other: A) : A = obj.join(a,other)

// Any composition of pointobjects is also a point object. ideally we'd be able to define this
// for compositions of variable arity
given [A](using obj: PointObject[A]) : PointObject[(A,A)] with
  def points(a: (A,A)) : Seq[Pt[Diff]] = obj.points(a._1) ++ obj.points(a._2)
  def mirror(a: (A,A), axis: Axis[Diff]): (A,A) = (a._1.mirror(axis), a._2.mirror(axis))
  def duplicate(a: (A,A)): (A,A) = (a._1.duplicate, a._2.duplicate)
  def join(a: (A,A), other: (A,A)): (A,A) = (a._1.join(other._1), a._2.join(other._2))

// given PointObject[Seq[Pt[Diff]]] with
//   def points(pts: Seq[Pt[Diff]]) = pts
//   def mirror(pts: Seq[Pt[Diff]], axis: Axis[Diff]) = pts.map(axis.reflect)
//   def duplicate(pts: Seq[Pt[Diff]]) = ???
//   def join(pts: Seq[Pt[Diff]], other: Seq[Pt[Diff]]) = pts ++ other

given [A] (using obj: PointObject[A]) : PointObject[Seq[A]] with
  def points(a: Seq[A]) : Seq[Pt[Diff]] = a.flatMap(obj.points)
  def mirror(pts: Seq[A], axis: Axis[Diff]) = ???
  def duplicate(pts: Seq[A]) = ???
  def join(pts: Seq[A], other: Seq[A]) = pts ++ other

given PointObject[Pt[Diff]] with
  def points(pt: Pt[Diff]) = Seq(pt)
  def mirror(pt: Pt[Diff], axis: Axis[Diff]) = axis.reflect(pt)
  def duplicate(pt: Pt[Diff]) = pt.duplicate
  def join(pt: Pt[Diff], o: Pt[Diff]) = ???


type Homogenous[H, T <: Tuple] = T match
  case EmptyTuple => DummyImplicit
  case H *: t => Homogenous[H, t]
  case _ => Nothing

def dist_L2(diffPt: Pt[Diff], target: Pt[Double])(using DiffContext) =
  val (distX, distY) = (diffPt.x - target.x, diffPt.y - target.y)
  (distX * distX) + (distY * distY)

def vertexLoss(vertices: Seq[Pt[Double]], diffVerts: Seq[Pt[Diff]], exclude : Int)(using DiffContext) =
  (0 until vertices.length).filter(k => k != exclude).map(k => dist_L2(diffVerts(k),vertices(k))).reduce(_+_)

def paramLoss(parameters: Seq[Diff], originalParameters: Seq[Double])(using DiffContext) : Diff =
  parameters.zip(originalParameters).foldLeft[Diff](0) { case (sum,(a,b)) =>
    val diff = b - a
    sum + (diff*diff)
  }


// DOM object isn't necessarily "one" object per se. If it's a collection it should be one that maintains its state.
case class Program[Params <: Tuple, Geometry, DOMObject](
  val parameters: Params,
  val execute : Params => Geometry,
  val initializeGeometry: (Params, Geometry) => DOMObject,
  val updateGeometry: (Params, Geometry, DOMObject) => Unit)
(using ctx: DiffContext, svg: SVGContext, h: Homogenous[Diff,Params], obj: PointObject[Geometry]) { self =>

  def apply(): Program[Params, Geometry, DOMObject] =
    var geometricStructure = execute(parameters)
    var diffPts = obj.points(geometricStructure)
    var concretePts = diffPts.map(_.map(_.primal))
    val elements = initializeGeometry(parameters,geometricStructure)


    // We know this is safe because of the homogenous parameter
    val ps = parameters.toList.toSeq.asInstanceOf[Seq[Diff]]
    val originalParameters = ps.map(_.primal)

    var vertices : Seq[Circle] = null;
    // var lastLoss :
    vertices = concretePts.zipWithIndex.map((pt,i) =>
      Circle(pt,"5px","transparent").draggable((target : Pt[Double]) =>
        val Pt(dx, dy) = diffPts(i)


        val loss = dist_L2(diffPts(i),target) + 0.1 * vertexLoss(concretePts, diffPts, i)
        ctx.prepare(Seq(loss))

        // We use a WASM-based version of SLSQP from the `nlopt-js` package
        val newParams = optimize(ps.map(_.primal),
          (newParams) => { ctx.update(ps, newParams); loss.primal },
          (newParams) => { ctx.update(ps, newParams); loss.d(ps, 1.0) }
        )

        val ploss = dist_L2(diffPts(i),target) + 0.1 * paramLoss(ps, originalParameters)
        ctx.prepare(Seq(ploss))
        val newParams2 = optimize(ps.map(_.primal),
          (newParams) => { ctx.update(ps, newParams); loss.primal },
          (newParams) => { ctx.update(ps, newParams); loss.d(ps, 1.0) }
        )


        // Update the path position and the vertex positions
        geometricStructure = execute(parameters)
        concretePts = obj.points(geometricStructure).map(_.map(_.primal))
        updateGeometry(parameters, geometricStructure, elements)
        for ((newPt,j) <- concretePts.zipWithIndex if j != i)
          vertices(j).setPosition(newPt)
      )
    )

    // Apply styling to be able to see the handle
    vertices.foreach(v => v.withClass("handle").attr("stroke","black"))
    self
}

case class Program2[Params <: Tuple, Geometry, DOMObject](
  val parameters: Params,
  val execute : Params => Geometry,
  val initializeGeometry: (Params, Geometry) => DOMObject,
  val updateGeometry: (Params, Geometry, DOMObject, Seq[Seq[Pt[Double]]]) => Unit)
(using ctx: DiffContext, svg: SVGContext, h: Homogenous[Diff,Params], obj: PointObject[Geometry]) { self =>
import js.JSConverters._

  private var _mode = 0
  def useParamLoss =
    _mode = 1
    self

  def apply(): Program2[Params, Geometry, DOMObject] =
    var geometricStructure = execute(parameters)
    var diffPts = obj.points(geometricStructure)
    var concretePts = diffPts.map(_.map(_.primal))
    val elements = initializeGeometry(parameters,geometricStructure)


    // We know this is safe because of the homogenous parameter
    val ps = parameters.toList.toSeq.asInstanceOf[Seq[Diff]]
    var originalParameters = ps.map(_.primal)

    var spt : Pt[Double] = Pt(0,0)
    var l = Path(Seq(Pt(0,0),Pt(0,0)))
      .attr("fill","transparent")
      .attr("stroke","blue")
      .attr("stroke-dasharray",2)

    var vertices : Seq[Circle] = null;
    vertices = concretePts.zipWithIndex.map((pt,i) =>
      Circle(pt,"5px","transparent").draggable((target : Pt[Double]) =>
        l.update(Seq(spt,target))
        val Pt(dx, dy) = diffPts(i)

        val currentParameters = ps.map(_.primal)
        val loss = dist_L2(diffPts(i),target) + 0.1 * vertexLoss(concretePts, diffPts, i)
        ctx.prepare(Seq(loss))

        // We use a WASM-based version of SLSQP from the `nlopt-js` package
        val newParams = optimize(ps.map(_.primal),
          (newParams) => { ctx.update(ps, newParams); loss.primal },
          (newParams) => { ctx.update(ps, newParams); loss.d(ps, 1.0) }
        )
        ctx.update(ps, newParams)
        val gs1 = execute(parameters)
        val pts1 = obj.points(gs1).concretePoints

        ctx.update(ps, originalParameters.toJSArray)

        val ploss = dist_L2(diffPts(i),target) + 0.1 * paramLoss(ps, originalParameters)
        ctx.prepare(Seq(ploss))
        val newParams2 = optimize(ps.map(_.primal),
          (newParams) => { ctx.update(ps, newParams); ploss.primal },
          (newParams) => { ctx.update(ps, newParams); ploss.d(ps, 1.0) }
        )

        ctx.prepare(diffPts.diffElements)
        ctx.update(ps, newParams2)
        val gs2 = execute(parameters)
        val pts2 = obj.points(gs2).concretePoints

        if (_mode == 0)     // VERTEX LOSS
          ctx.update(ps, newParams)
          geometricStructure = gs1
          concretePts = pts1
        else                // PARAM LOSS
          geometricStructure = gs2
          concretePts = pts2

        updateGeometry(parameters, geometricStructure, elements, Seq(pts1,pts2))
        for ((newPt,j) <- concretePts.zipWithIndex if j != i)
          vertices(j).setPosition(newPt)
      ,
      (pt) =>
          originalParameters = ps.map(_.primal)
          spt.set(pt)
      ,
      () =>
        l.update(Seq(Pt(0,0),Pt(0,0)))
    ))

    // Apply styling to be able to see the handle
    vertices.foreach(v => v.withClass("handle").attr("stroke","black"))
    self
}


case class EqProgram[Params <: Tuple, Geometry, DOMObject](
  val parameters: Params,
  val execute : (Params,EqualityContext[Diff]) => Geometry,
  val initializeGeometry: (Params, Geometry) => DOMObject,
  val updateGeometry: (Params, Geometry, DOMObject, Seq[Seq[Pt[Double]]]) => Unit)
(using ctx: DiffContext, svg: SVGContext, h: Homogenous[Diff,Params], obj: PointObject[Geometry]) { self =>
import js.JSConverters._
  def apply(): EqProgram[Params, Geometry, DOMObject] =
    val eqCtx = new EqualityContext[Diff]()

    var geometricStructure = execute(parameters, eqCtx)
    var diffPts = obj.points(geometricStructure)
    var concretePts = diffPts.map(_.map(_.primal))
    val elements = initializeGeometry(parameters,geometricStructure)

    // TODO implement
    self

}



def Mirror[Params <: Tuple, Geometry, DOMObject](
  p : Program[Params, Geometry, DOMObject],
  axis: Axis[Diff]
) (using ctx: DiffContext, svg: SVGContext, h: Homogenous[Diff,Params], obj: PointObject[Geometry]) =
  def splitList[A](list: Seq[A]) : (Seq[A],Seq[A]) =
    val l2 = (list.length / 2).toInt
    (list.slice(0,l2),list.slice(l2,list.length))

  val execute = (ps: Params) =>
    val geo = p.execute(ps)
    (geo, geo.mirror(axis))

  val initialize = (ps: Params, pts: (Geometry, Geometry)) =>
    val (l1,l2) = pts
    p.initializeGeometry(ps,l1) ++ p.initializeGeometry(ps,l2)

  val update = (ps: Params, pts: (Geometry,Geometry), geos: (DOMObject, DOMObject)) =>
    val (l1,l2) = pts
    val (g1,g2) = geos
    p.updateGeometry(ps,l1,g1)
    p.updateGeometry(ps,l2,g2)
  Program(p.parameters, execute, initialize, update)


case class Selector(var pts: Seq[Pt[Double]],
  onSelect: Set[Pt[Double]] => Any,
  onDeselect: Set[Pt[Double]] => Any
)
  (using ctx: SVGContext):
  private val dwg = ctx.current.dwg
  private var startClick = Pt[Double](0, 0)
  private var curClick = Pt[Double](0,0)
  private var rectangle = Path(Seq(startClick,startClick))
  rectangle
    .attr("stroke","black")
    .attr("stroke-dasharray",4)
    .attr("fill","#00000022")

  def inRectangle(pt: Pt[Double]) : Boolean =
    val minx = math.min(startClick.x,curClick.x)
    val miny = math.min(startClick.y,curClick.y)
    val maxx = math.max(startClick.x,curClick.x)
    val maxy = math.max(startClick.y,curClick.y)
    minx < pt.x && pt.x < maxx && miny < pt.y && pt.y < maxy

  private val moveListener: js.Function1[MouseEvent, Unit] = e =>
    curClick = Pt(e.clientX, e.clientY)
    rectangle.update(Seq(
      startClick,
      Pt(startClick.x, curClick.y),
      curClick,
      Pt(curClick.x, startClick.y),
      startClick
    ))

  val selected = Set[Pt[Double]]()

  private var upListener: js.Function1[MouseEvent, Unit] = null
  upListener = e => // Clean up
    dwg.removeEventListener("mousemove", moveListener)
    dwg.removeEventListener("mouseup", upListener)
    curClick = Pt(e.clientX, e.clientY)
    pts.map(c => if (inRectangle(c)) selected.add(c))
    onSelect(selected)
    rectangle.update(Seq())

  def clear =
    onDeselect(selected)
    selected.clear

  dwg.addEventListener("mousedown",(e: MouseEvent) =>
    if (!e.shiftKey) clear
    startClick = Pt(e.clientX, e.clientY)
    dwg.addEventListener("mousemove", moveListener)
    dwg.addEventListener("mouseup", upListener)
  )




given Draggable[Circle] with
  def basePoint(c: Circle) = c.position
  def element(c: Circle) = c.circ
  def render(c: Circle, loc: Pt[Double]): Unit = c.setPosition(loc)

given Draggable[Rectangle] with
  def basePoint(r: Rectangle) = r.p1
  def element(r: Rectangle) = r.path
  def render(r: Rectangle, loc: Pt[Double]) : Unit =
    r.update(loc,r.p2 + (loc-r.p1))

extension [A](a: A)(using drg: Draggable[A])
  def draggable =
    drg.draggable(a, _ => (), (_,_) => (), () => ())
  // Callback takes a simple position (no state). Good for simple drags.
  def draggable(onChange: Pt[Double] => Unit) =
    drg.draggable(a, _ => (), (p,_) => onChange(p), () => ())
  def draggable(onChange: Pt[Double] => Unit, onStart: Pt[Double] => Unit = _ => (), onEnd: () => Unit = () => ()) =
    drg.draggable(a,onStart,(p,_) => onChange(p),onEnd)
  // Callback takes a pair of position and differential from old location.
  def draggable(onChange: (Pt[Double], Pt[Double]) => Unit) =
    drg.draggable(a, _ =>(), onChange, () => ())

sealed trait Draggable[T]:
  def basePoint(geometry: T): Pt[Double]
  def element(geometry: T): Element
  def render(e: T, loc: Pt[Double]): Unit
  def draggable(geometry: T, onStart : Pt[Double] => Unit, onChange: (Pt[Double],Pt[Double]) => Unit, onEnd: () => Unit): T =
    var originalPt : Pt[Double] = Pt(0, 0)
    var originalClick = Pt[Double](0, 0)
    var elt = element(geometry)

    val doc = document
    // Needs to be explicitly typed otherwise the listener never moves
    // https://stackoverflow.com/questions/42748852/remove-event-listener-in-scala-js
    val moveListener: js.Function1[MouseEvent, Unit] = e =>
      val curClick = Pt(e.clientX, e.clientY)
      val diff = curClick - originalClick
      val controlPt = originalPt + diff
      render(geometry, controlPt)
      onChange(controlPt, diff)

    var upListener: js.Function1[MouseEvent, Unit] = null
    upListener = e => // Clean up
      doc.removeEventListener("mousemove", moveListener)
      doc.removeEventListener("mouseup", upListener)
      val _ = onEnd()

    elt.addEventListener( // Click on the element
      "mousedown",
      (e: MouseEvent) =>
        originalPt.set(basePoint(geometry))
        originalClick = Pt(e.clientX, e.clientY)
        val _ = onStart(originalPt)
        // Listen for it in the whole document.
        doc.addEventListener("mousemove", moveListener)
        doc.addEventListener("mouseup", upListener)
        e.stopPropagation
    )
    geometry


// Forwarding to the JS Canvas API. Ultimately, using our own context-function based API here
// will allow a custom DSL that is implemented by canvas without the user have to pass around
// the relevant state objects.
object PixelLayerFunctions:

  // Calculate the radius of the svg element explicitly according to the spec:
  // https://www.w3.org/TR/SVG/coords.html
  def Percent(value: Double)(using ctx: CanvasRenderingContext2D) =
    val w = dom.window.innerWidth
    val h = dom.window.innerHeight
    (value/100.0) * (math.sqrt(w * w + h * h) / math.sqrt(2.0))

  def moveTo(x : Double, y : Double)(using ctx: CanvasRenderingContext2D) = ctx.moveTo(x,y)
  def lineTo(x : Double, y : Double)(using ctx: CanvasRenderingContext2D) = ctx.lineTo(x,y)
  def lineWidth(w : Double)(using ctx: CanvasRenderingContext2D) = ctx.lineWidth = w
  def strokeStyle(style : String)(using ctx: CanvasRenderingContext2D) = ctx.strokeStyle = style
  def stroke()(using ctx: CanvasRenderingContext2D) = ctx.stroke()


object VectorLayerFunctions:
  def clip(using ctx: SVGContext)=  ()


object TestArity:
  given DiffContext = new DiffContext()

  def overFn[T <: Tuple](fn: T => Diff, t: T) =
    fn(t)

  def f1(a : Diff, b: Diff) =
    a + b
  val f4 = (a: Diff, b: Diff) => a+b

  val f2 : (Diff,Diff) = (10.v, 20.v)

  def f3(args: (Diff,Diff,Diff)) =
    val (a,b,c) = args
    a + b + c

  overFn(f1.tupled,f2)
  overFn(f3,(1.v,2.v,3.v))

class EqualityContext[T](using Operations[T]):
  import GenericTSyntax._
  val equalities = ArrayBuffer[T]()
  def fix(a: T, b: T) =
    equalities.append(a - b)


object TestConstraintProp:
  def run =
    given ctx: DiffContext = new DiffContext()
    val TOL = 1e-6


    case class Equal(left: Diff, right: Diff):
      // Conceptually we can think of this as taking an existing gradient
      // vector and re-aligning it to the closest value that satisfies the
      // constraint.
      def propagate(parameters: Seq[Diff], grad: Seq[Double]) =
        val originalParameters = parameters.map(p => p.primal)
        ctx.prepare(Seq(left,right))
        var tempGrad = ArrayBuffer.from(grad)

        // update the parameters with the negative gradient
        var updateParams = originalParameters.zip(tempGrad).map((p,g) => p-g)
        ctx.update(parameters.zip(updateParams):_*)
        // see if we've broken the constraint
        var differential = left.primal - right.primal

        var k = 0
        while (math.abs(differential) >= TOL && k < 5)
          println(s"differential = $differential")
          // Get a new derivative w/r/t this constraint, splitting
          // the responsibility equally among the two sides of the equality
          val steps = Seq(-differential*0.5, differential*0.5)
          val dG = ctx.d(parameters, Seq(left,right), steps)
          println(s"dG = $dG")

          // add it to the current gradient
          for (i <- 0 until tempGrad.length)
            tempGrad(i) += dG(i)
          updateParams = originalParameters.zip(tempGrad).map((p,g) => p-(g*0.1))
          // Re-check the differential
          ctx.update(parameters.zip(updateParams):_*)
          differential = left.primal - right.primal
          k += 1

        println(s"∇$tempGrad [k = $k]")
        // Converged
        tempGrad.toSeq

    val a = 3.v
    val b = 1.5.v

    val exp = (a*a)+b
    val ctr = Equal(a,2*b)

    val parameters = Seq(a,b)
    var parameterValues = parameters.map(_.primal)

    val objective = 9.0
    var k = 0
    while (math.abs(exp.primal - objective) >= TOL && k < 2)
      ctx.prepare(Seq(exp))
      val g = exp.d(parameters, 1)
      println(s"g = $g")
      val eq_g = ctr.propagate(parameters, g)
      parameterValues = parameterValues.zip(eq_g).map((p,g) => p-(g*0.001))
      ctx.prepare(Seq(exp))
      ctx.update(parameters.zip(parameterValues):_*)
      println(parameterValues)
      k += 1

object TestLagrangeMultipliers:
  def lagrange(parameters: Seq[Diff], expression: Diff, constraints: Seq[Diff])(using ctx: DiffContext) =
    val lagrangeParams = constraints.map(_ => 1.v)
    val lagrangeTerms = lagrangeParams.zip(constraints).map((p,c) => p*c).reduce(_+_)

    val objective = expression + lagrangeTerms
    ctx.prepare(Seq(objective))
    val optParams = parameters ++ lagrangeParams
    val result = optimize(optParams, objective)
    result.toSeq.take(parameters.length)

  def Equal(a: Diff, b: Diff)(using DiffContext) = (a-b).abs
  def SquareEqual(a : Diff, b: Diff)(using DiffContext) =
    val d = a*b
    d*d

  def run =
    given ctx: DiffContext = new DiffContext()
    val TOL = 1e-6

    def case1 =
      val a = 3.v
      val b = 1.5.v

      val expression = (a*a)+b
      val constraint = Equal(a,2*b)
      val target = 9.0

      val Seq(r_a,r_b) = lagrange(Seq(a,b),(expression - target).abs, Seq(constraint))
      ctx.update(a->r_a, b->r_b)
      println(s"Result = $r_a, $r_b [${expression.primal}]")

    def case2 =

      // We have more or less the following:
      // Two squares of side length r1, r2 respectively, with square 2 calculated as subtracting from a height of `t`.
      // We move the top right point of the upper square up one unit, and constrain the vertical distance between r1 and r2
      // to stay the same (2).
      //
      //           x  <- target
      // -  -  ____|
      // |  r2|    |
      // |  - |____|
      // t
      // |  -  ____
      // |  r1|    |
      // -  - |____|


      // parameters
      val r1 = 2.v
      val r2 = 2.v
      val t = 6.v

      // points
      val lowerPoints = Seq[Pt[Diff]](
        Pt(0.0,0.0),
        Pt(0+r1,0.0),
        Pt(r1,r1),
        Pt(0.0,0+r1)
      )
      val upperPoints = Seq[Pt[Diff]](
        Pt(0.0,t-r2),
        Pt(0+r2,t-r2),
        Pt(r2,t),
        Pt(0.0,t)
      )

      def distance(p1: Pt[Diff], p2: Pt[Diff]) =
        val dx = p2.x - p1.x
        val dy = p2.y - p1.y
        dx.abs + dy.abs

      // Not feasible.
      // (What we want to happen: constraint gets maintained, and the objective
      //  is satisfied as close as it possibly can be.)
      //
      // What actually happens: the lagrange solver gives some medium result between
      // satisfying the constraint and achieving the objective, and the other stuff
      val constraint_distance = SquareEqual(distance(lowerPoints(2),upperPoints(1)), 2.0)

      // Feasible. (Constraint gets maintained)
      val constraint_y = Equal((t-r2)-(r1),2)
      val objective = distance(upperPoints(2),Pt(2.0,7.0)) // drag out point



      println("L(x,λ)")
      var r = lagrange(Seq(r1,r2,t), objective, Seq(constraint))
      println(s"r1=${r(0)}, r2=${r(1)}, t=${r(2)} [objective = ${objective.primal}, constraint = ${constraint.primal}]")

      println("SLSQP - Singular")
      ctx.prepare(Seq(objective))
      r = optimizeEq(Seq(r1,r2,t), objective, Seq(constraint))
      println(s"r1=${r(0)}, r2=${r(1)}, t=${r(2)} [objective = ${objective.primal}, constraint = ${constraint.primal}]")

      println("SLSQP - Vector")
      ctx.prepare(Seq(objective))
      r = optimizeEqM(Seq(r1,r2,t), objective, Seq(constraint))
      println(s"r1=${r(0)}, r2=${r(1)}, t=${r(2)} [objective = ${objective.primal}, constraint = ${constraint.primal}]")

    case2

// L(x,λ)
// r1=1.076831057659771e+61, r2=1.0768310576509651e+61, t=-1.0749225866205047e+61 [objective = 2.3150236939486126e+122, constraint = 1.73847608626307e+247]
// Explodes entirely

// SLSQP - Singular
// r1=2.841332281541822, r2=2.8364102921420153, t=7.177375356835003 [objective = 0.7310441940134361, constraint = 80.92245370910834]

// not quite right, but getting there - the constraint is still far from being satisfied

// SLSQP - Vector
// r1=2.8427077710844446, r2=2.4740563482767937, t=6.626744924318656 [objective = 0.7276437451673053, constraint = 80.84317824870969]

// decides to shrink `t` for whatever dumb reason