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
import scala.scalajs.js.typedarray.Float32Array

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
  }

  case class Circle(center: Pt3, radius: Double, rot: Double = math.Pi / 4.0) extends Evaluable {
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

    def makeGeometry() = {
      val geometry = new BufferGeometry();

      val vDivs = 41
      val disc = 41

      val positions = new Float32Array(vDivs * disc * 3)
      val normals = new Float32Array(vDivs * disc * 3)
      val indices = new js.Array[Int]()

      def setPoint(positions: Float32Array)(i: Int, value: Pt3): Unit = {
        positions(3 * i) = value.x.toFloat
        positions(3 * i + 1) = value.y.toFloat
        positions(3 * i + 2) = value.z.toFloat
      }
      def getPoint(positions: Float32Array)(i: Int): Pt3 =
        Pt3(positions(3 * i), positions(3 * i + 1), positions(3 * i + 2))

      def setPosition = setPoint(positions) _
      def setNormal = setPoint(normals) _
      def getPosition = getPoint(positions) _
      def getNormal = getPoint(normals) _

      var R_CIRC = 10.0;
      var R_SQ = 5.0;
      var rot = math.Pi / 4.0;
      def compute() = {
        val rect = Rect(Pt3(R_SQ, R_SQ, -5), -2 * R_SQ, -2 * R_SQ)
        val circ = Circle(Pt3(0, 0, 5), R_CIRC, rot)
        // COMPUTE POSITIONS (parametric surface)
        for (i <- 0 until vDivs) {
          var t = i.toDouble / (vDivs - 1.0)
          var tz = cubic(t)
          val itz = 1.0 - tz
          val idx = disc * i;
          for (j <- 0 until disc) {
            val tx = j.toDouble / (disc - 1.0)
            val pt = rect.eval(tx) * (tz) + circ.eval(tx) * (itz);
            val nZ = t * rect.base.z + (1.0 - t) * circ.center.z
            setPosition(idx + j, Pt3(pt.x, pt.y, nZ))
            if (i < vDivs - 1) {
              val in = disc * (i + 1)
              val jn = (j + 1) % disc
              indices.push(idx + j, idx + jn, in + j, in + jn, in + j, idx + jn)
            }
          }
        }

        // COMPUTE NORMALS
        for (i <- 0 until vDivs) {
          for (j <- 0 until disc) {
            val idx = disc * i;
            val jp = (j - 1) mod disc; val ip = disc * (i - 1)
            val jn = (j + 1) mod disc; val in = disc * (i + 1)

            var agg = Pt3(0, 0, 0)
            var total = 0.0

            val a = getPosition(idx + j); val c = getPosition(idx + jn); val d = getPosition(idx + jp);
            if (i < vDivs - 1) {
              val b = getPosition(in + j); val e = getPosition(in + jp)
              agg += ((b - a) cross (c - a)) + ((a - b) cross (e - b)) + ((e - d) cross (a - d))
              total += 3.0
            }
            if (i > 0) {
              val b = getPosition(ip + j); val e = getPosition(ip + jn)
              agg += ((b - a) cross (d - a)) + ((a - b) cross (e - b)) + ((e - c) cross (a - c))
              total += 3.0
            }
            agg *= 1.0 / total
            setNormal(idx + j, agg)
          }
        }
        for (i <- 0 until vDivs) {
          val j0 = disc * i;
          val jn = disc * (i + 1) - 1;
          val n0 = getNormal(j0); val nn = getNormal(jn);
          val agg = n0.average(nn)
          setNormal(j0, agg); setNormal(jn, agg);
        }
      }

      compute()
      val posAttr = new BufferAttribute(positions, 3)
      geometry.setAttribute("position", posAttr)
      val normalAttr = new BufferAttribute(normals, 3)
      geometry.setAttribute("normal", normalAttr)

      def recompute = {
        compute()
        posAttr.needsUpdate = true
        normalAttr.needsUpdate = true
        render
      }

      document.addEventListener(
        "keydown",
        (e: KeyboardEvent) => {
          var needsRecompute = true;
          e.key match {
            case "a" => R_CIRC *= 0.9
            case "d" => R_CIRC *= 1.1
            case "w" => R_SQ *= 1.1
            case "s" => R_SQ *= 0.9
            case "r" => rot += 0.0628
            case "e" => rot -= 0.0628
            case _   => needsRecompute = false
          }
          if (needsRecompute) recompute
        }
      )

      geometry.setIndex(indices)
      geometry.computeBoundingBox()
      geometry
    }

    val geo = makeGeometry()
    val mat1 = new PointsMaterial(new PointsMaterialParameters { size = 0.1; color = "#000" })
    val mat2 = new MeshDepthMaterial(new MeshDepthMaterialParameters { wireframe = true })
    // val mat2 = new MeshNormalMaterial(new MeshNormalMaterialParameters { side = DoubleSide })
    val res1 = new Points(geo, mat1)
    val res2 = new Mesh(geo, mat2)
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
