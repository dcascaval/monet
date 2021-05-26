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
import scala.compiletime.ops.string
import scala.annotation.meta.param

case class Pt[T](val x : T, val y : T)(using Operations[T]):
  import GenericTSyntax._
  def +(other: Pt[T]) =
    Pt(x + other.x, y + other.y)

  def -(other: Pt[T]) =
    Pt(x - other.x, y - other.y)

  def *(other: Double) =
    Pt(x * other, y * other)

  def toSVG =
    s"${x.toInt} ${y.toInt}"

  def map[Q : Operations](f : T => Q): Pt[Q] =
    Pt(f(x),f(y))

def updateCircleCenter[T : Operations](element: Element, pt: Pt[T]) =
  import GenericTSyntax._
  element
    .attr("cx",pt.x.toInt)
    .attr("cy",pt.y.toInt)

def svg(tag: String) =
  document.createElementNS(SVG.URI, tag)

object SVG:
  val URI = "http://www.w3.org/2000/svg"

case class SVG()(using doc: Document):
  val dwg = svg("svg").asInstanceOf[SVGElement]
  val defs: ArrayBuffer[Gradient] = ArrayBuffer()
  val def_element = svg("defs")
  dwg.appendChild(def_element)

  def apply(): SVGElement = dwg
  def addDefinition(gradient: Gradient): Unit =
    defs.append(gradient)
    def_element.appendChild(gradient.element)

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
  ctx.current.addDefinition(self)
}

case class Mask(id: String, artwork: Element => Any)(using ctx: SVGContext):
  val element = svg("mask")
    .attr("id", id)
    .child(
       svg("rect")
        .attr("fill" -> "white",
              "width" -> "100%",
              "height" -> "100%"))
  artwork(element)
  ctx.current.def_element.appendChild(element)

sealed trait Layer { self =>
  def element: Element
  def draw(w: Double, h: Double): Layer = self
  def attr[A](key: String, value: A) : Layer =
    element.setAttributeNS(null, key, s"${value}")
    self
}


extension [T <: Element](elt: T)
  def withClass(cls: String) : T =
    elt.classList.add(cls)
    elt
  def withStyle(styles: (String,Any)*) =
    val text = styles.map((k,v) => s"${k}:${v};").reduce(_+_)
    elt.attr("style",text)
    elt
  def attr[Q](key: String, value: Q): T =
    elt.setAttributeNS(null, key, value.toString)
    elt
  def attr(mapping: (String,Any)*): T =
    for ((k,v) <- mapping) attr(k,v)
    elt
  def child[Q <: Element](makeChild: => Q) =
    elt.appendChild(makeChild)
    elt
  def draw()(using ctx: SVGContext) =
    ctx.current.dwg.appendChild(elt)

extension (canvas : HTMLCanvasElement)
  def context2D : CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

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

given Conversion[Circle, Element] with
  def apply(p: Circle): Element = p.circ

case class Circle(var position: Pt[Double], val radius: String, val fill: String | Gradient)
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
  def mask(m: Mask): Circle =
    circ.attr("mask",s"url(#${m.id})")
    self
  def setPosition(p : Pt[Double]) =
    updateCircleCenter(circ, p)
    position = p
}

given Conversion[Path, Element] with
  def apply(p: Path): Element = p.path

object Path {
  def empty(using SVGContext) : Path = Path(Seq())
}

case class Path(var points: Seq[Pt[Double]])(using ctx: SVGContext) { self =>
  val path = svg("path")

  def update(newPoints: Seq[Pt[Double]]) =
    points = newPoints
    if (newPoints.length > 0)
      var pathString = s"M ${newPoints(0).toSVG}"
      var rest = newPoints.drop(1).map(p => s"L ${p.toSVG}").reduce(_+_)
      path
        .attr("d",pathString+rest)


  update(points)
  path.draw()
}

case class Axis[T : Operations](a: Pt[T], b: Pt[T]):
  import GenericTSyntax._
  val diff = b.x - a.x
  val slope = (b.y - a.y) / diff
  val yInt = (b.x*a.y - a.x*b.y) / diff
  val s2 = (slope*slope) + 1.0

  def reflect(c: Pt[T]) =
    val d = ((c.x + (c.y - yInt)*slope) / s2) * 2.0
    Pt[T](d - c.x, d * slope - c.y + yInt*2)

trait Geometrizable:
  def duplicate: Geometrizable
  def mirror[T:Operations](c: Axis[T]): Geometrizable

trait Geometry[A]:
  def duplicate(a: A): A
  def mirror[T:Operations](a: A, c: Axis[T]): A

given (using SVGContext): Geometry[Circle] with
  def duplicate(circle: Circle) =
    Circle(circle.position, circle.radius, circle.fill)
  def mirror[T:Operations](circle: Circle, a: Axis[T]) = // fake fake not real
    Circle(circle.position, circle.radius, circle.fill)
  //   Circle()


// class CC(val b: Pt, val r: Double)
// class SS(val pts: Seq[Pt])

// given Geometry[CC] with
//   def duplicate(a: CC) = CC(a.b,a.r)
//   def mirror(a: CC, c: Double) = CC(a.b, a.r+c)

// given Geometry[SS] with
//   def duplicate(a: SS) = SS(a.pts)
//   def mirror(a: SS, c: Double) = SS(a.pts.map(p => Pt(p.x+c,p.y)))

// We want to be polymorphic over a list of potentially non-homonogenous geometries.
// As a result we would either have to subclass some abstract geometry class to do it,
// but then we would be stuck doing some F-bounded polymorphism thing and I don't want to deal,
// particularly because 99% of the time we don't even really need the specialized methods
given mkGeometrizable[A](using ops: Geometry[A]) : Conversion[A,Geometrizable] with
  def apply(a: A): Geometrizable =
    new Geometrizable {
      def duplicate = ops.duplicate(a)
      def mirror[T:Operations](c: Axis[T]) = ops.mirror(a,c)
    }

// Each program takes as input a set of initial parameters that serve as the root
// of our optimization, a function to execute to find the positions of the control
// points, and a path function that redraws the path given a combination of parameters
// and control points.
//
// This avoids duplicating work in the case where the path _is_ the control points,
// and allows for rendering to multiple paths etc. It is up to the author of the
// path function what level of bidirectionality they want to expose.


type Homogenous[H, T <: Tuple] = T match
  case EmptyTuple => DummyImplicit
  case H *: t => Homogenous[H, t]
  case _ => Nothing

case class Program[Params <: Tuple](
  parameters: Params,
  execute : Params => Seq[Pt[Diff]],
  makePath: (Params, Seq[Pt[Double]]) => Unit)
(using ctx: DiffContext, svg: SVGContext, h: Homogenous[Diff,Params]) { self =>
  var MAX_ITERS = 10
  var SCALE_GRADIENT = MAX_ITERS

  var diffPts = execute(parameters)
  var concretePts = diffPts.map(_.map(_.primal))
  makePath(parameters, concretePts)

  var vertices : Seq[Circle] = null;
  vertices = concretePts.zipWithIndex.map((pt,i) =>
    Circle(pt,"5px","transparent").draggable((target : Pt[Double]) =>
      val Pt(dx, dy) = diffPts(i)
      val (distX, distY) = (dx - target.x, dy - target.y)
      val dist = (distX * distX) + (distY * distY)
      ctx.prepare(Seq(dist))
      var prevDist = dist.primal

      // We know this is safe because of the homogenous parameter
      val ps = parameters.toList.toSeq.asInstanceOf[Seq[Diff]]

      // We use a WASM-based version of SLSQP from the `nlopt-js` package
      val newParams = optimize(ps.map(_.primal),
        (newParams) => { ctx.update(ps, newParams); dist.primal },
        (newParams) => { ctx.update(ps, newParams); dist.d(ps, 1.0) }
      )
      ctx.update(ps, newParams)

      // Update the path position and the vertex positions
      diffPts = execute(parameters)
      concretePts = diffPts.map(_.map(_.primal))
      makePath(parameters, concretePts)
      for ((newPt,j) <- concretePts.zipWithIndex if j != i)
        vertices(j).setPosition(newPt)
    )
  )

  // Apply styling to be able to see the handle
  vertices.foreach(v => v.withClass("handle").attr("stroke","black"))
}

given Draggable[Circle] with
  def basePoint(c: Circle) = c.position
  def element(c: Circle) = c.circ
  def render(c: Circle, loc: Pt[Double]): Unit = c.setPosition(loc)

extension [A](a: A)(using drg: Draggable[A])
  def draggable = drg.draggable(a, (p) => ())
  def draggable(onChange: Pt[Double] => Unit) = drg.draggable(a, onChange)

sealed trait Draggable[T]:
  def basePoint(geometry: T): Pt[Double]
  def element(geometry: T): Element
  def render(e: T, loc: Pt[Double]): Unit
  def draggable(geometry: T, onChange: Pt[Double] => Unit): T =
    var originalPt : Pt[Double] = Pt(0, 0)
    var originalClick = Pt[Double](0, 0)
    var elt = element(geometry)

    val doc = document
    // Needs to be explicitly typed otherwise the listener never moves
    // https://stackoverflow.com/questions/42748852/remove-event-listener-in-scala-js
    val moveListener: js.Function1[MouseEvent, Unit] = e =>
      val curClick = Pt(e.clientX, e.clientY)
      val controlPt = originalPt + (curClick - originalClick)
      render(geometry, controlPt)
      onChange(controlPt)

    var upListener: js.Function1[MouseEvent, Unit] = null
    upListener = e => // Clean up
      doc.removeEventListener("mousemove", moveListener)
      doc.removeEventListener("mouseup", upListener)

    elt.addEventListener( // Click on the element
      "mousedown",
      (e: MouseEvent) =>
        originalPt = basePoint(geometry)
        originalClick = Pt(e.clientX, e.clientY)
        // Listen for it in the whole document.
        doc.addEventListener("mousemove", moveListener)
        doc.addEventListener("mouseup", upListener)
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