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
// import scala.collection.mutable.ArrayBuffer

object ThreeExamplePoints {
  def raytest() = {
    import typings.three._
    import typings.three.THREE._
    import scalajs.js.typedarray._

    val π = math.Pi;
    val mouse = new Vector2();
    val spheres = scala.collection.mutable.ArrayBuffer[Mesh]();
    val threshold = 0.1
    val pointSize = 0.05
    val width = 80
    val length = 160
    val rotateY = new Matrix4().makeRotationY(0.0005)

    // SOME HELPERS

    def generatePointCloudGeometry(
        color: Color,
        width: Int,
        length: Int
    ) = {
      val geometry = new BufferGeometry();
      val numPoints = width * length;
      val positions = new Float32Array(numPoints * 3);
      val colors = new Float32Array(numPoints * 3);

      var k = 0;
      for (i <- 0 until width) {
        for (j <- 0 until length) {
          val u = i.toFloat / width;
          val v = j.toFloat / length;
          val x = u - 0.5f;
          val y = ((math.cos(u * π * 4) + math.sin(v * π * 8)) / 20.0).toFloat;
          val z = v - 0.5f;

          positions(3 * k) = x;
          positions(3 * k + 1) = y;
          positions(3 * k + 2) = z;

          val intensity = (y + 0.1) * 5;
          colors(3 * k) = (color.r * intensity).toFloat;
          colors(3 * k + 1) = (color.g * intensity).toFloat;
          colors(3 * k + 2) = (color.b * intensity).toFloat;
          k += 1;
        }
      }

      geometry.setAttribute("position", new BufferAttribute(positions, 3));
      geometry.setAttribute("color", new BufferAttribute(colors, 3));
      geometry.computeBoundingBox()
      geometry
    }

    def generatePointcloud(color: Color, width: Int, length: Int) = {
      val geometry = generatePointCloudGeometry(color, width, length);
      val material = new PointsMaterial(
        new PointsMaterialParameters {
          size = pointSize;
          vertexColors = true
        }
      )
      new Points(geometry, material);
    }

    def generateIndexedPointcloud(color: Color, width: Int, length: Int) = {
      val geometry = generatePointCloudGeometry(color, width, length);
      val numPoints = width * length;
      val indices = new Uint16Array(numPoints);

      var k = 0;
      for (i <- 0 until width) {
        for (j <- 0 until length) {
          indices(k) = k; k += 1
        }
      }

      geometry.setIndex(new BufferAttribute(indices, 1));
      val material = new PointsMaterial(
        new PointsMaterialParameters {
          size = pointSize;
          vertexColors = true
        }
      );
      new Points(geometry, material)
    }

    def generateIndexedWithOffsetPointcloud(
        color: Color,
        width: Int,
        length: Int
    ) = {

      val geometry = generatePointCloudGeometry(color, width, length);
      val numPoints = width * length;
      val indices = new Uint16Array(numPoints);
      var k = 0;
      for (i <- 0 until width) {
        for (j <- 0 until length) {
          indices(k) = k; k += 1
        }
      }

      geometry.setIndex(new BufferAttribute(indices, 1));
      geometry.addGroup(0, indices.length);
      val material = new PointsMaterial(
        new PointsMaterialParameters {
          size = pointSize;
          vertexColors = true
        }
      );
      new Points(geometry, material)
    }

    // INITIALIZATION ROUTINE

    val window = dom.window
    val container = document.createElement("div")
    container.classList.add("three-layer")
    document.body.appendChild(container)

    val scene = new Scene();
    scene.background = new Color(0xffffff);
    val clock = new Clock();

    val camera = new PerspectiveCamera(
      45,
      window.innerWidth / window.innerHeight,
      1,
      10000
    );
    camera.position.set(10, 10, 10);
    camera.lookAt(scene.position);
    camera.updateMatrix();

    val pcBuffer =
      generatePointcloud(new Color(1, 0, 0), width, length);
    pcBuffer.scale.set(5, 10, 10);
    pcBuffer.position.set(-5, 0, 0);
    scene.add(pcBuffer);

    val pcIndexed =
      generateIndexedPointcloud(new Color(0, 1, 0), width, length);
    pcIndexed.scale.set(5, 10, 10);
    pcIndexed.position.set(0, 0, 0);
    scene.add(pcIndexed);

    val pcIndexedOffset = generateIndexedWithOffsetPointcloud(
      new Color(0, 1, 1),
      width,
      length
    );
    pcIndexedOffset.scale.set(5, 10, 10);
    pcIndexedOffset.position.set(5, 0, 0);
    scene.add(pcIndexedOffset);

    val pointclouds =
      js.Array(pcBuffer, pcIndexed, pcIndexedOffset);

    val sphereGeometry = new SphereGeometry(0.1, 32, 32);
    val sphereMaterial = new MeshBasicMaterial(
      new MeshBasicMaterialParameters { color = 0xff0000 }
    );

    for (i <- 0 until 40) {
      val sphere = new Mesh(sphereGeometry, sphereMaterial);
      scene.add(sphere);
      spheres.addOne(sphere);
    }
    val renderer = new WebGLRenderer(
      new WebGLRendererParameters { antialias = true }
    );
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(window.innerWidth, window.innerHeight);
    container.appendChild(renderer.domElement);
    val raycaster = new Raycaster();
    raycaster.params.Points.threshold = threshold;

    val onWindowResize = (e: Event) => {
      camera.aspect = window.innerWidth / window.innerHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(window.innerWidth, window.innerHeight);
    }

    // ANIMATION LOOP

    var toggle = 0.0;
    var spheresIndex = 0;

    def render() = {
      camera.applyMatrix4(rotateY);
      camera.updateMatrixWorld();
      raycaster.setFromCamera(mouse, camera);
      val intersections = raycaster.intersectObjects(pointclouds);
      val intersection =
        if ((intersections.length) > 0) intersections(0) else null;

      if (toggle > 0.02 && (intersection != null)) {
        spheres(spheresIndex).position.copy(intersection.point);
        spheres(spheresIndex).scale.set(1, 1, 1);
        spheresIndex = (spheresIndex + 1) % spheres.length;
        toggle = 0;
      }

      for (sphere <- spheres) {
        sphere.scale.multiplyScalar(0.98);
        sphere.scale.clampScalar(0.01, 1);
      }

      toggle += clock.getDelta();
      renderer.render(scene, camera);
    }

    var animate: scala.scalajs.js.Function1[Double, _] = null;
    animate = (_: Double) => {
      window.requestAnimationFrame(animate);
      render();
    }

    val onDocumentMouseMove = (event: MouseEvent) => {
      event.preventDefault();
      mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
      mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;
    }

    window.addEventListener("resize", onWindowResize);
    document.addEventListener("mousemove", onDocumentMouseMove);
    animate(0.0)
  }
}
