package textedit

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.dom.raw.CanvasRenderingContext2D
import org.scalajs.dom.raw.Event
import org.scalajs.dom.Element
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.raw.SVGElement
import org.scalajs.dom.svg
import org.scalajs.dom.raw.Document
import scala.collection.mutable.ArrayBuffer

case class Pt(val x: Double, val y: Double) {
  def +(other: Pt) = {
    Pt(x + other.x, y + other.y)
  }
  def -(other: Pt) = {
    Pt(x - other.x, y - other.y)
  }
  def *(other: Double) = {
    Pt(x * other, y * other)
  }
}

object Circle {
  def updateCircleCenter(element: Element, pt: Pt) = {
    element.setAttributeNS(null, "cx", s"${pt.x.toInt}")
    element.setAttributeNS(null, "cy", s"${pt.y.toInt}")
  }
}
import Circle._

object SVG {
  val URI = "http://www.w3.org/2000/svg"
}

case class SVG()(implicit doc: Document) {
  val dwg = document.createElementNS(SVG.URI, "svg").asInstanceOf[SVGElement]
  val defs: ArrayBuffer[Gradient] = ArrayBuffer()
  val def_element = document.createElementNS(SVG.URI, "defs")
  dwg.appendChild(def_element)

  def apply(): SVGElement = dwg
  def addDefinition(gradient: Gradient): Unit = {
    defs.append(gradient)
    def_element.appendChild(gradient.element)
  }
}

sealed trait Gradient {
  val element: Element
}

case class RadialGradient(name: String, stops: (Int, String)*)(implicit
    ctx: SVGContext
) extends Gradient { self =>
  private val sortStops = stops.toVector.sortBy { case (a, _) => a }

  val element = document.createElementNS(SVG.URI, "radialGradient")
  element.setAttributeNS(null, "id", "rg1")
  element.setAttributeNS(null, "gradientUnits", "userSpaceOnUse")
  for ((percent, color) <- sortStops) {
    val stopElement = document.createElementNS(SVG.URI, "stop")
    stopElement.setAttributeNS(null, "offset", s"${percent}%")
    stopElement.setAttributeNS(null, "stop-color", color)
    element.appendChild(stopElement)
  }

  ctx.current.addDefinition(self)
}

sealed trait Layer {
  def element: Element
  def draw(w: Double, h: Double): Unit = ()
}

sealed class SVGContext {
  val layers: ArrayBuffer[SVG] = ArrayBuffer()
  def push(v: SVG) = layers.append(v)
  def current = layers.last
}

object Layers {
  def VectorLayer(
      artwork: => Any
  )(implicit ctx: SVGContext, doc: Document): Layer = {
    val svg = SVG()
    svg().classList.add("vec-layer");
    svg().setAttributeNS(null, "style", "opacity:0.9;")
    ctx.push(svg)
    artwork
    new Layer {
      val element = svg()
    }
  }

  def PixelLayer(
      drawCallback: (CanvasRenderingContext2D, Double, Double) => Unit
  )(implicit
      doc: Document
  ): Layer = {
    var w = dom.window.innerWidth
    var h = dom.window.innerHeight
    val canvas = doc.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.classList.add("pix-layer");
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    val layer = new Layer {
      val element = canvas
      override def draw(w: Double, h: Double) = drawCallback(ctx, w, h)
    }
    layer.draw(w, h)
    layer
  }

}
import Layers._

case class Circle(val position: Pt, radius: String, fill: Gradient)(implicit
    ctx: SVGContext
) {
  val circ = document.createElementNS(SVG.URI, "circle")
  updateCircleCenter(circ, position)
  circ.setAttributeNS(null, "r", "10%");
  circ.setAttributeNS(null, "fill", "url(#rg1)")
  ctx.current.dwg.appendChild(circ)
}

object DraggableImpls {
  implicit val dragCircle = new Draggable[Circle] {
    def basePoint(c: Circle) = c.position
    def element(c: Circle) = c.circ
    def render(e: Element, loc: Pt): Unit = updateCircleCenter(e, loc)
  }

  implicit class DraggableSyntax[A](a: A)(implicit drg: Draggable[A]) {
    def draggable = drg.draggable(a)
  }

}
import DraggableImpls._

sealed trait Draggable[T] {
  def basePoint(geometry: T): Pt
  def element(geometry: T): Element
  def render(e: Element, loc: Pt): Unit
  def draggable(geometry: T): T = {
    var controlPt = basePoint(geometry)
    var originalPt = basePoint(geometry)
    var elt = element(geometry)
    var originalClick = Pt(0, 0)

    val doc = document
    // Needs to be explicitly typed otherwise the listener never moves
    // https://stackoverflow.com/questions/42748852/remove-event-listener-in-scala-js
    val moveListener: js.Function1[MouseEvent, Unit] = e => {
      val curClick = Pt(e.clientX, e.clientY)
      controlPt = originalPt + (curClick - originalClick)
      render(elt, controlPt)
    }

    var upListener: js.Function1[MouseEvent, Unit] = null;
    upListener = e => { // Clean up
      doc.removeEventListener("mousemove", moveListener)
      doc.removeEventListener("mouseup", upListener)
    }

    elt.addEventListener( // Click on the element
      "mousedown",
      (e: MouseEvent) => {
        originalPt = controlPt
        originalClick = Pt(e.clientX, e.clientY)

        // Listen for it in the whole document.
        doc.addEventListener("mousemove", moveListener)
        doc.addEventListener("mouseup", upListener)
      }
    )
    geometry
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", (e: Event) => render())
  }

  def txt(text: String, classes: String*): Element = {
    val e = document.createElement("p")
    e.textContent = text
    e.classList.add("spaced")
    e
  }

  def render() = {

    implicit val doc = document
    implicit val svgctx = new SVGContext()
    document.body.classList.add("center")
    val p = txt("MONET", "spaced")

    var w = dom.window.innerWidth
    var h = dom.window.innerHeight

    val pixlayer = PixelLayer({
      case (ctx, w, h) => {
        for (t <- 0 to 100) {
          val dt = t.toDouble / 100.0
          ctx.moveTo(dt * w, 0);
          ctx.lineTo(w / 2, h / 2);
        }
        ctx.lineWidth = 0.5;
        ctx.strokeStyle = "#aaa";
        ctx.stroke();
      }
    })

    val svg = VectorLayer {
      val g = RadialGradient("rg1", 0 -> "#9b78ff", 100 -> "#51c9e2")
      def mkcirc(x: Int, y: Int) = {
        var circPt = Pt(x, y)
        var circ = Circle(circPt, "10%", g).draggable
      }
      mkcirc(250, 250)
      mkcirc(750, 750)
    }

    val layers = Seq(
      svg,
      pixlayer
    )

    // When the canvas is resized we need to tweak.
    def setLayerSize(layer: Layer): Layer = {
      layer.element.setAttributeNS(null, "width", s"${w.toInt}")
      layer.element.setAttributeNS(null, "height", s"${h.toInt}")
      layer
    }
    def drawLayer(layer: Layer): Layer = {
      layer.draw(w, h); layer
    }

    dom.window.addEventListener(
      "resize",
      (_: Event) => {
        w = dom.window.innerWidth
        h = dom.window.innerHeight
        layers.map(setLayerSize).map(drawLayer)
      }
    )

    document.body.appendChild(p)
    layers.map(layer => {
      layers.map(setLayerSize).map(drawLayer)
      document.body.appendChild(layer.element)
    })

  }
}
