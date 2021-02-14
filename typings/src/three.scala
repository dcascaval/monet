package typings.three

import scalajs.js
import scalajs.js.|
import scalajs.js.annotation._
import org.scalajs.dom._
import org.scalajs.dom.raw.{HTMLCanvasElement, WebGLRenderingContext}

@js.native
@JSGlobal
class OffscreenCanvas extends js.Object

class WebGLRendererParameters(
    var canvas: js.UndefOr[HTMLCanvasElement | OffscreenCanvas] = js.undefined,
    var precision: js.UndefOr[String] = js.undefined,
    var alpha: js.UndefOr[Boolean] = js.undefined,
    var premultipliedAlpha: js.UndefOr[Boolean] = js.undefined,
    var antialias: js.UndefOr[Boolean] = js.undefined,
    var stencil: js.UndefOr[Boolean] = js.undefined,
    var preserveDrawingBuffer: js.UndefOr[Boolean] = js.undefined,
    var powerPreference: js.UndefOr[String] = js.undefined,
    var depth: js.UndefOr[Boolean] = js.undefined,
    var logarithmicDepthBuffer: js.UndefOr[Boolean] = js.undefined,
    var failIfMajorPerformanceCaveat: js.UndefOr[Boolean] = js.undefined
) extends js.Object {}

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
    var matrixAutoUpdate: Boolean = js.native;
    def updateMatrix(): Unit = js.native;
    def add(objs: Object3D*): Unit = js.native;
    def lookAt(
        vector: Vector3 | Double,
        y: js.UndefOr[Double] = js.undefined,
        z: js.UndefOr[Double] = js.undefined
    ): Unit = js.native
  }

  @js.native
  class Vector3(var x: Double = 0.0, var y: Double = 0.0, var z: Double = 0.0)
      extends js.Object {}

  @js.native
  class Euler(
      var x: Double = 0.0,
      var y: Double = 0.0,
      var z: Double = 0.0,
      var order: js.UndefOr[String] = js.undefined
  ) extends js.Object {}

  @js.native
  class Scene extends Object3D {
    var background: Color = js.native
    var fog: Fog = js.native
  }

  @js.native
  class Fog(c: Color | Double | String, near: Double = 1.0, far: Double = 2000)
      extends js.Object {}

  @js.native
  class Group extends Object3D {}

  @js.native
  class Color extends js.Object {
    def this(c: Color | String | Double) = this
    def this(r: Double, g: Double, b: Double) = this
  }

  @js.native
  class BufferGeometry extends js.Object {}

  @js.native
  class BoxGeometry(width: Double, height: Double, depth: Double)
      extends BufferGeometry {}

  @js.native
  class Material extends js.Object {}

  @js.native
  class MeshNormalMaterial extends Material {}

  @js.native
  class Mesh(geometry: BufferGeometry, material: Material | js.Array[Material])
      extends Object3D {}

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
  class WebGLRenderer(params: js.UndefOr[WebGLRendererParameters])
      extends Renderer {
    def setPixelRatio(value: Double): Unit = js.native
  }

}
