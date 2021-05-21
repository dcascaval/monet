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

case class Pt(val x: Double, val y: Double):
  def +(other: Pt) =
    Pt(x + other.x, y + other.y)

  def -(other: Pt) =
    Pt(x - other.x, y - other.y)

  def *(other: Double) =
    Pt(x * other, y * other)

  def toSVG =
    s"${x.toInt} ${y.toInt}"

object Circle:
  def updateCircleCenter(element: Element, pt: Pt) =
    element
      .attr("cx",pt.x.toInt)
      .attr("cy",pt.y.toInt)

import Circle._

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

case class Circle(val position: Pt, radius: String, fill: String | Gradient)(using ctx: SVGContext) { self =>
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
}

given Conversion[Path, Element] with
  def apply(p: Path): Element = p.path

case class Path(var points: ArrayBuffer[Pt])(using ctx: SVGContext) { self =>
  val path = svg("path")

  def update(newPoints: ArrayBuffer[Pt]) =
    points = newPoints
    if (newPoints.length > 0)
      var pathString = s"M ${newPoints(0).toSVG}"
      var rest = newPoints.drop(1).map(p => s"L ${p.toSVG}").reduce(_+_)
      path
        .attr("d",pathString+rest)
        .attr("fill","black")

  update(points)
  path.draw()
}

given Draggable[Circle] with
  def basePoint(c: Circle) = c.position
  def element(c: Circle) = c.circ
  def render(e: Element, loc: Pt): Unit = updateCircleCenter(e, loc)

extension [A](a: A)(using drg: Draggable[A])
  def draggable = drg.draggable(a, (p) => ())
  def draggable(onChange: Pt => Unit) = drg.draggable(a, onChange)

sealed trait Draggable[T]:
  def basePoint(geometry: T): Pt
  def element(geometry: T): Element
  def render(e: Element, loc: Pt): Unit
  def draggable(geometry: T, onChange: Pt => Unit): T =
    var controlPt = basePoint(geometry)
    var originalPt = basePoint(geometry)
    var elt = element(geometry)
    var originalClick = Pt(0, 0)

    val doc = document
    // Needs to be explicitly typed otherwise the listener never moves
    // https://stackoverflow.com/questions/42748852/remove-event-listener-in-scala-js
    val moveListener: js.Function1[MouseEvent, Unit] = e =>
      val curClick = Pt(e.clientX, e.clientY)
      controlPt = originalPt + (curClick - originalClick)
      render(elt, controlPt)
      onChange(controlPt)

    var upListener: js.Function1[MouseEvent, Unit] = null
    upListener = e => // Clean up
      doc.removeEventListener("mousemove", moveListener)
      doc.removeEventListener("mouseup", upListener)

    elt.addEventListener( // Click on the element
      "mousedown",
      (e: MouseEvent) =>
        originalPt = controlPt
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