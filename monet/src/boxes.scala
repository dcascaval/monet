package monet

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.Element
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.svg
import org.scalajs.dom.raw.Document
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.typedarray._

import typings.three.THREE._
import java.nio.Buffer

object ThreeBlend {

  implicit class posMod(x: Int) {
    def mod(m: Int): Int = {
      val r = x % m
      if (r < 0) r + m else r
    }
  }

  case class Pt3(val x: Double, val y: Double, val z: Double) { self =>
    def +(other: Pt3) = {
      Pt3(x + other.x, y + other.y, z + other.z)
    }
    def -(other: Pt3) = {
      Pt3(x - other.x, y - other.y, z - other.z)
    }
    def *(other: Double) = {
      Pt3(x * other, y * other, z * other)
    }
    def cross(b: Pt3) = {
      val result = Pt3(y * b.z - z * b.y, -(x * b.z - z * b.x), x * b.y - y * b.x)
      result * (1.0 / length)
    }
    def length = {
      math.sqrt(x * x + y * y + z * z)
    }
    def average(other: Pt3) = (self + other) * 0.5
  }

  trait Evaluable {
    def eval(parameter: Double): Pt3
    val center: Pt3
  }

  case class Circle(val center: Pt3, radius: Double, rot: Double = math.Pi / 4.0) extends Evaluable {
    def eval(parameter: Double): Pt3 = {
      val θ = (parameter * 2 * math.Pi) + rot;
      val rx = center.x + radius * math.cos(θ)
      val ry = center.y + radius * math.sin(θ)
      Pt3(rx, ry, center.z)
    }
  }

  case class Rect(base: Pt3, x: Double, y: Double) extends Evaluable {
    def eval(parameter: Double): Pt3 = {
      def root(center: Double) =
        math.max(0, math.min(1, 4.0 * ((-math.abs(center - parameter)) + 0.375)))
      val dx = x * root(0.375)
      val dy = y * root(0.625)
      Pt3(base.x + dx, base.y + dy, base.z)
    }
    val center = Pt3(base.x + 0.5 * x, base.y + 0.5 * y, base.z)
  }

  class Blend(shapes: Evaluable*) {
    var iters = 0
    var vDivs = 41
    var disc = 41
    var objects = shapes.toVector

    def setShapes(shapes: Evaluable*): Unit = {
      objects = shapes.toVector
    }

    trait Mesher {
      val posAttr: BufferAttribute
      val normalAttr: BufferAttribute
      def getPosition(i: Int): Pt3
      def getNormal(i: Int): Pt3
      def setPosition(i: Int, value: Pt3): Unit
      def setNormal(i: Int, value: Pt3): Unit
    }

    var wireFrameGeo = new BufferGeometry();
    var geo = new BufferGeometry();

    def generateMesher: Mesher = {
      val pos = new Float32Array(vDivs * disc * 3)
      val norm = new Float32Array(vDivs * disc * 3)
      val idx = new js.Array[Double]()

      def setPoint(positions: Float32Array)(i: Int, value: Pt3): Unit = {
        positions(3 * i) = value.x.toFloat
        positions(3 * i + 1) = value.y.toFloat
        positions(3 * i + 2) = value.z.toFloat
      }

      def getPoint(positions: Float32Array)(i: Int): Pt3 =
        Pt3(positions(3 * i), positions(3 * i + 1), positions(3 * i + 2))

      for (i <- 0 until vDivs) {
        val ii = disc * i;
        for (j <- 0 until disc) {
          if (i < vDivs - 1) {
            val in = disc * (i + 1)
            val jn = (j + 1) % disc
            idx.push(ii + j, ii + jn, in + j, in + jn, in + j, ii + jn)
          }
        }
      }

      if (iters == 0) {
        // This wonderful state of affairs is due to:
        // https://github.com/mrdoob/three.js/issues/20933,
        // wherein we cannot call `setIndex()` twice on standard mesh geometry, when it is
        // wireframe because this screws up the indexing.
        //
        // BUT, on normal geometry, we _have_ to call setIndex multiple times to get
        // the buffers to resize correctly.
        // As a result if we want to display wireframes we have to duplicate the data.
        wireFrameGeo.setIndex(idx)
        geo.setIndex(idx)
      } else {
        geo.setIndex(idx)
        wireFrameGeo.index.array = new Uint32Array(idx)
        wireFrameGeo.index.count = idx.length
        wireFrameGeo.index.needsUpdate = true
      }

      iters += 1

      new Mesher {
        val posAttr = new BufferAttribute(pos, 3)
        val normalAttr = new BufferAttribute(norm, 3)
        def getPosition(i: Int): Pt3 = getPoint(pos)(i)
        def setPosition(i: Int, v: Pt3) = setPoint(pos)(i, v)
        def getNormal(i: Int): Pt3 = getPoint(norm)(i)
        def setNormal(i: Int, v: Pt3) = setPoint(norm)(i, v)
      }
    }

    var mesher = generateMesher

    def computePoints = {
      objects match {
        case Vector(g1, g2) =>
          // COMPUTE POSITIONS (parametric surface)
          for (i <- 0 until vDivs) {
            var t = i.toDouble / (vDivs - 1.0)
            var tz = cubic(t)
            val itz = 1.0 - tz
            val idx = disc * i;
            for (j <- 0 until disc) {
              val tx = j.toDouble / (disc - 1.0)
              val pt = g1.eval(tx) * (tz) + g2.eval(tx) * (itz);
              val nZ = t * g1.center.z + (1.0 - t) * g2.center.z
              mesher.setPosition(idx + j, Pt3(pt.x, pt.y, nZ))
            }
          }
        case _ => ()
      }
    }

    def computeNormals = {
      // COMPUTE NORMALS
      for (i <- 0 until vDivs) {
        for (j <- 0 until disc) {
          val idx = disc * i;
          val jp = (j - 1) mod disc; val ip = disc * (i - 1)
          val jn = (j + 1) mod disc; val in = disc * (i + 1)

          var agg = Pt3(0, 0, 0)
          var total = 0.0

          val a = mesher.getPosition(idx + j); val c = mesher.getPosition(idx + jn);
          val d = mesher.getPosition(idx + jp);
          if (i < vDivs - 1) {
            val b = mesher.getPosition(in + j); val e = mesher.getPosition(in + jp)
            agg += ((b - a) cross (c - a)) + ((a - b) cross (e - b)) + ((e - d) cross (a - d))
            total += 3.0
          }
          if (i > 0) {
            val b = mesher.getPosition(ip + j); val e = mesher.getPosition(ip + jn)
            agg += ((b - a) cross (d - a)) + ((a - b) cross (e - b)) + ((e - c) cross (a - c))
            total += 3.0
          }
          agg *= 1.0 / total
          mesher.setNormal(idx + j, agg)
        }
      }
      for (i <- 0 until vDivs) {
        val j0 = disc * i;
        val jn = disc * (i + 1) - 1;
        val n0 = mesher.getNormal(j0); val nn = mesher.getNormal(jn);
        val agg = n0.average(nn)
        mesher.setNormal(j0, agg); mesher.setNormal(jn, agg);
      }
    }

    def replaceAttribute(name: String, attr: BufferAttribute) = {
      if (wireFrameGeo.hasAttribute(name)) {
        wireFrameGeo.deleteAttribute(name)
        geo.deleteAttribute(name)
      }
      wireFrameGeo.setAttribute(name, attr)
      geo.setAttribute(name, attr)
      attr.needsUpdate = true
    }

    // TODO: check if we need to push needsUpdate = true here because we're creating
    // the buffer attributes before we're actually initializing the underlying data
    def geometry = {
      computePoints
      computeNormals
      replaceAttribute("position", mesher.posAttr)
      replaceAttribute("normal", mesher.normalAttr)
      wireFrameGeo.computeBoundingBox()
      geo.computeBoundingBox()
      (wireFrameGeo, geo)
    }

    def recalculatePositions = {
      computePoints
      computeNormals
      mesher.posAttr.needsUpdate = true
      mesher.normalAttr.needsUpdate = true
    }

    def recalculateGeometry(verticalDivisons: Int, horizontalDisc: Int) = {
      vDivs = verticalDivisons
      disc = horizontalDisc
      mesher = generateMesher
      geometry
      wireFrameGeo.computeBoundingBox()
      geo.computeBoundingBox()
    }

  }

  def cubic(t: Double) = {
    val ti = (1 - t)
    val t2 = t * t
    val t3 = t2 * t
    (3 * ti * t2) + t3
  }

  def blend = {
    import typings.three._
    import typings.three.THREE._
    import scalajs.js.typedarray._

    val window = dom.window
    val aspect = window.innerWidth / window.innerHeight
    val container = document.createElement("div")
    container.classList.add("grad-bg")
    document.body.appendChild(container)

    val scene = new Scene()
    // scene.background = new Color("#000000")
    val camera = new PerspectiveCamera(45, aspect, 1.0, 100)
    camera.position.set(10, 10, 10)
    camera.lookAt(scene.position)
    camera.updateMatrix()

    val renderer = new WebGLRenderer(new WebGLRendererParameters { antialias = true; alpha = true })
    val controls = new OrbitControls(camera, renderer.domElement)
    renderer.setPixelRatio(window.devicePixelRatio)
    renderer.setSize(window.innerWidth, window.innerHeight)
    container.appendChild(renderer.domElement)
    controls.update()

    def render = {
      renderer.render(scene, camera);
    }

    var updateMesh: Unit => Unit = (_: Unit) => ();

    def makeGeometry() = {

      var vd = 41; var hd = 41;
      var R_CIRC = 10.0;
      var R_SQ = 5.0;
      var rot = math.Pi / 4.0;

      def shapes = {
        val rect = Rect(Pt3(R_SQ, R_SQ, -5), -2 * R_SQ, -2 * R_SQ)
        val circ = Circle(Pt3(0, 0, 5), R_CIRC, rot)
        (rect, circ)
      }

      val (r, c) = shapes
      val blend = new Blend(r, c)

      def recomputePositions = {
        val (r, c) = shapes
        blend.setShapes(r, c)
        blend.recalculatePositions
        render
      }

      def recomputeGeometry = {
        blend.recalculateGeometry(vd, hd)
        render
      }

      document.addEventListener(
        "keydown",
        (e: KeyboardEvent) => {
          var needsRecompute = true;
          def regen = {
            recomputeGeometry
            needsRecompute = false
          }
          e.key match {
            case "a" => R_CIRC *= 0.9
            case "d" => R_CIRC *= 1.1
            case "w" => R_SQ *= 1.1
            case "s" => R_SQ *= 0.9
            case "r" => rot += 0.0628
            case "e" => rot -= 0.0628
            case "o" => vd -= 1; regen
            case "p" => vd += 1; regen
            case "k" => hd -= 1; regen
            case "l" => hd += 1; regen
            case _   => needsRecompute = false
          }
          if (needsRecompute) recomputePositions
        }
      )

      blend.geometry
    }

    val (wireFrameGeo, geo) = makeGeometry()
    val mat1 = new PointsMaterial(new PointsMaterialParameters { size = 0.1; color = "#FFF" })
    val isWireframe = true
    // val mat2 = new MeshDepthMaterial(new MeshDepthMaterialParameters { side = DoubleSide })
    val mat2 = new MeshNormalMaterial(new MeshNormalMaterialParameters { wireframe = true; side = DoubleSide })

    val res1 = new Points(geo, mat1)
    var res2 = new Mesh(if (isWireframe) wireFrameGeo else geo, mat2)
    scene.add(res1, res2)

    def onWindowResize(e: Event) = {
      camera.aspect = window.innerWidth / window.innerHeight
      camera.updateProjectionMatrix()
      renderer.setSize(window.innerWidth, window.innerHeight)
      render
    }

    renderer.render(scene, camera)
    window.addEventListener("resize", onWindowResize);
    controls.addEventListener("change", (e: Event) => render)
    render
  }
}

object ThreeBoxes {
  def threejs() = {
    import typings.three._
    import typings.three.THREE._

    val window = dom.window;
    var mouseX = 0.0; var mouseY = 0.0;
    var windowHalfX = window.innerWidth / 2;
    var windowHalfY = window.innerHeight / 2;
    var render: () => Unit = () => ();

    def init() = {

      val camera = new PerspectiveCamera(
        60,
        window.innerWidth / window.innerHeight,
        1,
        10000
      );
      camera.position.z = 2000;

      val scene = new Scene();
      scene.background = new Color(0xffffff);
      scene.fog = new Fog(0xffffff, 1, 10000);

      val geometry = new BoxGeometry(100, 100, 100);
      val material = new MeshNormalMaterial();

      val group = new Group();

      for (i <- 0 until 100) {
        val mesh = new Mesh(geometry, material);
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

      val params = new WebGLRendererParameters { antialias = true }
      val renderer = new WebGLRenderer(params);
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
      window.requestAnimationFrame(animate);
      render();
    }

    init();
    animate(0.0);
  }
}
