package monet

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
import monet.three.THREE.WebGLRenderer
import monet.three.THREE.WebGLRendererParameters

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

  // TODO: ultimately we will want to incorporate the monaco editor with some light scala-like parsing features (via: fastparse? scalameta?) and bidirectional editing functions.
  def render() = {
    threejs()

    implicit val doc = document
    implicit val svgctx = new SVGContext()
    document.body.classList.add("center")
    // TODO: text layer API, something better for specifying classes.
    val p = txt("MONET", "spaced")

    var w = dom.window.innerWidth
    var h = dom.window.innerHeight

    val circPoints = Seq((250, 250, 0.1), (750, 750, 0.1))

    // TODO: port the reactive layer from mimic so we can just tweak the values directly.
    // TODO: we want something like the VectorLayer API here, and not to be passing w,h in directly.
    // TODO: add an API for clipping that gets compiled to clip-path properties on the canvas object
    // TODO: add an API for opacity
    val pixlayer = PixelLayer({
      case (ctx, w, h) => {
        // Calculate the radius of the 10% svg element explicitly according to the spec:
        // https://www.w3.org/TR/SVG/coords.html
        val cx = 250.0; val cy = 250.0; val px = 0.1;
        val r = px * (math.sqrt(w * w + h * h) / math.sqrt(2.0));
        println(r);

        // Find the tangent from the border point to the circle.
        // TODO: add a web-assembly based 2d geometry library
        def targetPoint(sx: Double, sy: Double) = {
          val dx = sx - cx; val dy = sy - cy;
          val (dxr, dyr) = (-dy, dx)
          val d = math.sqrt(dx * dx + dy * dy)
          if (d >= r) {
            val rho = r / d
            val ad = rho * rho
            val bd = rho * math.sqrt(1 - rho * rho)
            val t1x = cx + ad * dx + bd * dxr
            val t1y = cy + ad * dy + bd * dyr
            val t2x = cx + ad * dx - bd * dxr
            val t2y = cy + ad * dy - bd * dyr
            (t1x, t1y)
          } else (sx, sy)
        }

        def radialLine(sx: Double, sy: Double) = {
          ctx.moveTo(sx, sy)
          val (tx, ty) = targetPoint(sx, sy)
          ctx.lineTo(tx, ty)
        }
        for (t <- 0 until 100) {
          val dt = t.toDouble / 100.0
          radialLine(dt * w, 0)
          radialLine(w, dt * h)
          radialLine((1.0 - dt) * w, h)
          radialLine(0, (1.0 - dt) * h)
        }
        ctx.lineWidth = 0.5;
        ctx.strokeStyle = "#aaa";
        ctx.stroke();
      }
    })

    // TODO: figure out how extracting the objects for a cross-layer API might work --
    //       i.e. collect the created objects from the execution context.
    val svg = VectorLayer {
      // TODO: get a better color API. strings can be colors, but so can... you know, more descriptive representations.
      val g = RadialGradient("rg1", 0 -> "#9b78ff", 100 -> "#51c9e2")
      def mkcirc(x: Int, y: Int) = {
        var circPt = Pt(x, y)
        // TODO: parametric percentages are a function of width.
        Circle(circPt, "10%", g).draggable
      }
      mkcirc(250, 250)
      mkcirc(750, 750)
    }

    // TODO: add three.js based 3d layers.
    val layers = Seq(svg, pixlayer)

    // TODO: move the layer utilities elsewhere and allow us just to specify the layer sequence here.

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

  def threejs() = {
    import monet.three._

    def boxGeometry(width: Double, height: Double, depth: Double) = {
      new THREE.BoxGeometry(width, height, depth)
    }

    val window = dom.window;
    var mouseX = 0.0; var mouseY = 0.0;
    var windowHalfX = window.innerWidth / 2;
    var windowHalfY = window.innerHeight / 2;
    var render: () => Unit = () => ();

    def init() = {

      val camera = new THREE.PerspectiveCamera(
        60,
        window.innerWidth / window.innerHeight,
        1,
        10000
      );
      camera.position.z = 2000;

      val scene = new THREE.Scene();
      scene.background = new THREE.Color(0xffffff);
      // scene.fog = new THREE.Fog(0xffffff, 1, 10000);

      val geometry = boxGeometry(100, 100, 100);
      val material = new THREE.MeshNormalMaterial();

      val group = new THREE.Group();

      for (i <- 0 until 100) {
        val mesh = new THREE.Mesh(geometry, material);
        mesh.position.x = math.random() * 2000 - 1000;
        mesh.position.y = math.random() * 2000 - 1000;
        mesh.position.z = math.random() * 2000 - 1000;
        mesh.rotation.x = math.random() * 2 * math.Pi;
        mesh.rotation.y = math.random() * 2 * math.Pi;

        mesh.matrixAutoUpdate = false;
        mesh.updateMatrix();
        group.add(mesh);
      }

      scene.add(group);

      val params = new THREE.WebGLRendererParameters {}
      params.antialias = true
      val renderer = new THREE.WebGLRenderer(params);
      renderer.setPixelRatio(window.devicePixelRatio);
      renderer.setSize(window.innerWidth, window.innerHeight);
      renderer.domElement.classList.add("three-layer");
      document.body.appendChild(renderer.domElement);

      def onWindowResize(e: Event) = {

        windowHalfX = window.innerWidth / 2;
        windowHalfY = window.innerHeight / 2;

        camera.aspect = window.innerWidth / window.innerHeight;
        camera.updateProjectionMatrix();

        renderer.setSize(window.innerWidth, window.innerHeight);
      }

      def onDocumentMouseMove(event: MouseEvent) = {
        mouseX = (event.clientX - windowHalfX) * 10;
        mouseY = (event.clientY - windowHalfY) * 10;
      }

      // document.addEventListener("mousemove", onDocumentMouseMove)
      dom.window.addEventListener("resize", onWindowResize)

      render = () => {

        val time = System.nanoTime() * 0.0000000001;

        val rx = Math.sin(time * 0.7) * 0.5;
        val ry = Math.sin(time * 0.3) * 0.5;
        val rz = Math.sin(time * 0.2) * 0.5;

        camera.position.x += (mouseX - camera.position.x) * 0.05;
        camera.position.y += (-mouseY - camera.position.y) * 0.05;

        camera.lookAt(scene.position);

        group.rotation.x = rx;
        group.rotation.y = ry;
        group.rotation.z = rz;
        renderer.render(scene, camera);
      }
    }

    var animate: scala.scalajs.js.Function1[Double, _] = null;
    animate = (_: Double) => {
      // window.requestAnimationFrame(animate);
      render();
    }

    init();
    animate(0.0);
  }
}
