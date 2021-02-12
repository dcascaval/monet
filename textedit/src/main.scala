package textedit

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.dom.raw.CanvasRenderingContext2D
import org.scalajs.dom.raw.Event
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.raw.SVGElement
import org.scalajs.dom.svg

class Pt(val x: Double, val y: Double) {
  def +(other: Pt) = {
    new Pt(x + other.x, y + other.y)
  }
  def -(other: Pt) = {
    new Pt(x - other.x, y - other.y)
  }
  def *(other: Double) = {
    new Pt(x * other, y * other)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", (e: Event) => render())
  }

  def render() = {

    println("Hello world!")
    val p = document.createElement("p")
    p.textContent = "MONET"
    document.body.classList.add("center")
    p.classList.add("spaced")

    var w = dom.window.innerWidth
    var h = dom.window.innerHeight

    val layer1 =
      document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    layer1.classList.add("pix-layer");

    val SVG_URI = "http://www.w3.org/2000/svg"
    val svg1 =
      document.createElementNS(SVG_URI, "svg").asInstanceOf[SVGElement]
    svg1.classList.add("vec-layer");

    val defs = document.createElementNS(SVG_URI, "defs")
    val rg1 = document.createElementNS(SVG_URI, "radialGradient")
    rg1.setAttributeNS(null, "id", "rg1")
    rg1.setAttributeNS(null, "gradientUnits", "userSpaceOnUse")
    val s1 = document.createElementNS(SVG_URI, "stop")
    s1.setAttributeNS(null, "offset", "0%")
    s1.setAttributeNS(null, "stop-color", "#9b78ff")
    val s2 = document.createElementNS(SVG_URI, "stop")
    s2.setAttributeNS(null, "offset", "100%")
    s2.setAttributeNS(null, "stop-color", "#51c9e2")
    rg1.appendChild(s1)
    rg1.appendChild(s2)
    defs.appendChild(rg1)
    svg1.appendChild(defs)

    var circPt = new Pt(250, 250)

    val circ = document.createElementNS(SVG_URI, "circle")
    def drawCirc() = {
      circ.setAttributeNS(null, "cx", s"${circPt.x.toInt}")
      circ.setAttributeNS(null, "cy", s"${circPt.y.toInt}")
    }
    drawCirc()
    circ.setAttributeNS(null, "r", "10%");
    circ.setAttributeNS(null, "fill", "url(#rg1)")
    svg1.appendChild(circ)

    //
    {
      var originalPt = circPt
      var originalClick = new Pt(0, 0)

      val doc = document
      // Needs to be explicitly typed otherwise the listener never moves
      // https://stackoverflow.com/questions/42748852/remove-event-listener-in-scala-js
      val moveListener: js.Function1[MouseEvent, Unit] = e => {
        // println("move")
        val curClick = new Pt(e.clientX, e.clientY)
        circPt = originalPt + (curClick - originalClick)
        drawCirc()
      }

      var upListener: js.Function1[MouseEvent, Unit] = null;
      upListener = e => { // Clean up
        // println("clean")
        doc.removeEventListener("mousemove", moveListener)
        doc.removeEventListener("mouseup", upListener)
      }

      circ.addEventListener( // Click on the element
        "mousedown",
        (e: MouseEvent) => {
          // println("click")
          originalPt = circPt
          originalClick = new Pt(e.clientX, e.clientY)

          // Listen for it in the whole document.
          doc.addEventListener("mousemove", moveListener)
          doc.addEventListener("mouseup", upListener)
        }
      )
    }

    val layers = Seq(
      svg1,
      layer1
    )

    // When the canvas is resized we need to tweak.
    def setLayerSize() = {
      println("updating")
      layers.map(layer => {
        layer.setAttributeNS(null, "width", s"${w.toInt}")
        layer.setAttributeNS(null, "height", s"${h.toInt}")
      })
    }

    def drawCanvas() = {
      val ctx = layer1.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
      for (t <- 0 to 100) {
        val dt = t.toDouble / 100.0
        ctx.moveTo(dt * w, 0);
        ctx.lineTo(w / 2, h / 2);
      }
      ctx.lineWidth = 0.5;
      ctx.strokeStyle = "#aaa";
      ctx.stroke();
    }

    setLayerSize()
    drawCanvas()

    dom.window.addEventListener(
      "resize",
      (_: Event) => {
        w = dom.window.innerWidth
        h = dom.window.innerHeight
        setLayerSize()
        drawCanvas()
      }
    )

    document.body.appendChild(p)
    layers.map(layer => document.body.appendChild(layer))
  }
}
