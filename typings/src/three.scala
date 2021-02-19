package typings.three

import scala.scalajs.js
import js.|
import js.annotation._

import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLCanvasElement, WebGLRenderingContext}
import scalajs.js.typedarray._
import javax.sound.sampled.Line
import java.lang.reflect.Parameter
import typings.three.THREE.Camera
import org.scalajs.dom.raw.HTMLElement

@js.native
@JSGlobal
class OffscreenCanvas extends js.Object

// PARAMETER CLASSES:
// Nominally, these are part of THREE, but they are actually just interfaces, not classes,
// and are never directly instantiated by name in JS --
// they are structural types for bag-of-properties inputs to various THREE functions.
trait WebGLRendererParameters extends js.Object {
  var canvas: js.UndefOr[HTMLCanvasElement | OffscreenCanvas] = js.undefined
  var precision: js.UndefOr[String] = js.undefined
  var alpha: js.UndefOr[Boolean] = js.undefined
  var premultipliedAlpha: js.UndefOr[Boolean] = js.undefined
  var antialias: js.UndefOr[Boolean] = js.undefined
  var stencil: js.UndefOr[Boolean] = js.undefined
  var preserveDrawingBuffer: js.UndefOr[Boolean] = js.undefined
  var powerPreference: js.UndefOr[String] = js.undefined
  var depth: js.UndefOr[Boolean] = js.undefined
  var logarithmicDepthBuffer: js.UndefOr[Boolean] = js.undefined
  var failIfMajorPerformanceCaveat: js.UndefOr[Boolean] = js.undefined
}

trait MaterialParameters extends js.Object {
  var vertexColors: js.UndefOr[Boolean] = js.undefined
  var side: js.UndefOr[THREE.Side] = js.undefined
}

trait LineMaterialParameters extends MaterialParameters {
  var color: js.UndefOr[Double] = js.undefined
  var dashed: js.UndefOr[Boolean] = js.undefined
  var dashScale: js.UndefOr[Double] = js.undefined
  var dashSize: js.UndefOr[Double] = js.undefined
  var dashOffset: js.UndefOr[Double] = js.undefined
  var gapSize: js.UndefOr[Double] = js.undefined
  var linewidth: js.UndefOr[Double] = js.undefined
  var resolution: js.UndefOr[THREE.Vector2] = js.undefined
}

trait PointsMaterialParameters extends MaterialParameters {
  var color: js.UndefOr[THREE.Color | String | Double] = js.undefined
  var map: js.UndefOr[THREE.Texture | Null] = js.undefined
  var alphaMap: js.UndefOr[THREE.Texture | Null] = js.undefined
  var size: js.UndefOr[Double] = js.undefined
  var sizeAttenuation: js.UndefOr[Boolean] = js.undefined
  var morphTargets: js.UndefOr[Boolean] = js.undefined
}

trait MeshBasicMaterialParameters extends MaterialParameters {
  var color: js.UndefOr[THREE.Color | String | Double] = js.undefined
}

trait MeshDepthMaterialParameters extends MaterialParameters {
  var wireframe: js.UndefOr[Boolean] = js.undefined
  var wireframeLinewidth: js.UndefOr[Double] = js.undefined
}

trait MeshNormalMaterialParameters extends MaterialParameters {}

@js.native
@JSGlobal
object THREE extends js.Object {

  @js.native
  class PerspectiveCamera(
      var fov: Double = 50,
      var aspect: Double = 1.0,
      var near: Double = 0.1,
      var far: Double = 2000
  ) extends Camera {
    def updateProjectionMatrix(): Unit = js.native
  }

  @js.native
  class Camera extends Object3D {}

  @js.native
  class Object3D extends js.Object {
    val position: Vector3 = js.native;
    val rotation: Euler = js.native;
    val scale: Vector3 = js.native;
    var matrixAutoUpdate: Boolean = js.native;
    def updateMatrix(): Unit = js.native;
    def add(objs: Object3D*): Unit = js.native;
    def lookAt(
        vector: Vector3 | Double,
        y: js.UndefOr[Double] = js.undefined,
        z: js.UndefOr[Double] = js.undefined
    ): Unit = js.native
    def applyMatrix4(matrix: Matrix4): Unit = js.native
    def updateMatrixWorld(force: js.UndefOr[Boolean] = js.undefined): Unit =
      js.native;
  }

  @js.native
  class Vector3(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0) extends js.Object {
    def set(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0): Unit = js.native
    def copy(v: Vector3): Unit = js.native
    def multiplyScalar(s: Double): Vector3 = js.native
    def clampScalar(min: Double, max: Double): Vector3 = js.native
  }

  @js.native
  class Vector2(var x: Double = 0.0, var y: Double = 0.0) extends js.Object {}

  @js.native
  class Euler(
      var x: Double = 0.0,
      var y: Double = 0.0,
      var z: Double = 0.0,
      var order: js.UndefOr[String] = js.undefined
  ) extends js.Object {}

  @js.native
  class Matrix extends js.Object {}

  @js.native
  class Matrix4 extends Matrix {
    def makeRotationX(theta: Double): Matrix4 = js.native
    def makeRotationY(theta: Double): Matrix4 = js.native
    def makeRotationZ(theta: Double): Matrix4 = js.native
  }

  @js.native
  class Scene extends Object3D {
    var background: Color = js.native
    var fog: Fog = js.native
  }

  @js.native
  class Fog(c: Color | Double | String, near: Double = 1.0, far: Double = 2000) extends js.Object {}

  @js.native
  class Group extends Object3D {}

  @js.native
  class Points(
      geometry: js.UndefOr[BufferGeometry] = js.undefined,
      material: js.UndefOr[Material | js.Array[Material]] = js.undefined
  ) extends Object3D {}

  @js.native
  class Color extends js.Object {
    def this(c: Color | String | Double) = this
    def this(r: Double, g: Double, b: Double) = this
    var r: Double = js.native
    var g: Double = js.native
    var b: Double = js.native
  }

  @js.native
  class BufferGeometry extends js.Object {
    def setAttribute(
        name: String,
        attribute: BufferAttribute
    ): Unit = js.native
    def setIndex(
        index: BufferAttribute | js.Array[Int] | Null
    ): BufferGeometry = js.native
    def addGroup(
        start: Double,
        count: Double,
        materialIndex: js.UndefOr[Double] = js.undefined
    ): Unit = js.native
    def computeBoundingBox(): Unit = js.native
    def computeVertexNormals(): Unit = js.native
  }

  // GEOMETRIES
  @js.native
  class BoxGeometry(width: Double, height: Double, depth: Double) extends BufferGeometry {}

  @js.native
  class CircleGeometry extends BufferGeometry {}
  @js.native
  class CylinderGeometry extends BufferGeometry {}
  @js.native
  class EdgesGeometry extends BufferGeometry {}
  @js.native
  class ExtrudeGeometry extends BufferGeometry {}
  @js.native
  class LatheGeometry extends BufferGeometry {}
  @js.native
  class ParametricGeometry(
      func: js.Function3[Double, Double, Vector3, Unit],
      slices: Int,
      stacks: Int
  ) extends BufferGeometry {}
  @js.native
  class PlaneGeometry extends BufferGeometry {}
  @js.native
  class PolyhedronGeometry extends BufferGeometry {}
  @js.native
  class RingGeometry extends BufferGeometry {}
  @js.native
  class SphereGeometry(
      radius: js.UndefOr[Double] = js.undefined,
      widthSegments: js.UndefOr[Double] = js.undefined,
      heightSegments: js.UndefOr[Double] = js.undefined,
      phiStart: js.UndefOr[Double] = js.undefined,
      phiLength: js.UndefOr[Double] = js.undefined,
      thetaStart: js.UndefOr[Double] = js.undefined,
      thetaLength: js.UndefOr[Double] = js.undefined
  ) extends BufferGeometry {}
  @js.native
  class TorusGeometry extends BufferGeometry {}
  @js.native
  class TubeGeometry extends BufferGeometry {}
  @js.native
  class WireframeGeometry extends BufferGeometry {}

  //
  //
  // MATERIALS
  //
  //
  @js.native
  class Material extends js.Object {}

  // MISC
  @js.native
  class LineBasicMaterial extends Material {}
  @js.native
  class PointsMaterial extends Material {
    def this(parameters: PointsMaterialParameters) = this
  }
  @js.native
  class ShaderMaterial extends Material {}
  @js.native
  class ShadowMaterial extends Material {}
  @js.native
  class SpriteMaterial extends Material {}

  // MESH MATERIALS
  @js.native
  class MeshBasicMaterial extends Material {
    def this(parameters: MeshBasicMaterialParameters) = this
  }
  @js.native
  class MeshDepthMaterial extends Material {
    def this(parameters: MeshDepthMaterialParameters) = this
  }
  @js.native
  class MeshDistanceMaterial extends Material {}
  @js.native
  class MeshLambertMaterial extends Material {}
  @js.native
  class MeshMatcapMaterial extends Material {}
  @js.native
  class MeshNormalMaterial extends Material {
    def this(parameters: MeshNormalMaterialParameters) = this
  }

  @js.native
  class MeshPhongMaterial extends Material {}
  @js.native
  class MeshStandardMaterial extends Material {}
  @js.native
  class MeshToonMaterial extends Material {}

  @js.native
  class Texture extends js.Object {}

  @js.native
  class Mesh(geometry: BufferGeometry, material: Material | js.Array[Material]) extends Object3D {}

  @js.native
  trait Renderer extends js.Object {
    var domElement: HTMLCanvasElement = js.native
    def render(scene: Object3D, camera: Camera): Unit = js.native
    def setSize(
        width: Double,
        height: Double,
        updateStyle: js.UndefOr[Boolean] = js.undefined
    ): Unit = js.native
  }

  @js.native
  class WebGLRenderer(params: js.UndefOr[WebGLRendererParameters]) extends Renderer {
    def setPixelRatio(value: Double): Unit = js.native
  }

  type NumArrayLike = js.Array[Double] | Float32Array | Uint16Array

  @js.native
  class BufferAttribute(
      var array: NumArrayLike,
      var itemSize: Double,
      var normalized: Boolean = false
  ) extends js.Object {
    var needsUpdate: Boolean = js.native
  }

  @js.native
  trait Thresholdable extends js.Object {
    var threshold: Double = js.native
  }

  @js.native
  trait RaycasterParameters extends js.Object {
    val Points: Thresholdable = js.native
  }

  @js.native
  class Raycaster extends js.Object {
    val params: RaycasterParameters = js.native
    def setFromCamera(coords: Vector2 | Vector3, camera: Camera): Unit =
      js.native
    def intersectObjects[T <: Object3D](
        objects: js.Array[T],
        recursive: js.UndefOr[Boolean] = js.undefined,
        optionalTarget: js.UndefOr[js.Array[Intersection]] = js.undefined
    ): js.Array[Intersection] = js.native
  }

  @js.native
  trait Intersection extends js.Object {
    val distance: Double = js.native;
    val point: Vector3 = js.native
    val `object`: Object3D = js.native
    val distanceToRay: js.UndefOr[Double] = js.native;

    val index: js.UndefOr[Int] = js.native
    val face: js.UndefOr[Face | Null] = js.native
    val faceIndex: js.UndefOr[Int] = js.native
    val uv: js.UndefOr[Vector2] = js.native
    val instanceId: js.UndefOr[Int] = js.native
  }

  @js.native
  trait Face extends js.Object {
    val a: Int = js.native
    val b: Int = js.native
    val c: Int = js.native
    val normal: Vector3 = js.native
    val materialIndex: Int = js.native
  }

  @js.native
  class Clock extends js.Object {
    def getDelta(): Double = js.native
  }

  @js.native
  sealed trait Side extends js.Object {}

  val FrontSide: Side = js.native
  val BackSide: Side = js.native
  val DoubleSide: Side = js.native

  // EXTRAS
  @js.native
  class OrbitControls(camera: THREE.Camera, domElement: HTMLElement) extends js.Object {
    def update(): Unit = js.native
    def addEventListener(t: String, e: js.Function1[Event, Unit]): Unit = js.native
  }
}
