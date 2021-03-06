package typings.nlopt

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal
object nlopt extends js.Object:

  def ready : js.Promise[js.Any] = js.native

  type Algorithm

  @js.native
  class NLOptResult extends js.Object:
    val x : js.Array[Double] = js.native  // Minimized parameters
    val success: Boolean = js.native
    val value: Double = js.native         // Function value at minimum

  @js.native
  object Algorithm extends js.Object:
    val LD_SLSQP : Algorithm = js.native

  @js.native
  class Optimize(algorithm: Algorithm, dimension: Int) extends js.Object:
    def setMinObjective(
      fn : js.Function2[js.Array[Double],js.Array[Double],Double],
      tolerance: Double
    ) : Double = js.native

    def optimize(parameters: js.Array[Double]) : NLOptResult = js.native

    def addEqualityConstraint(
      fn: js.Function2[js.Array[Double],js.Array[Double],Double],
      tolerance: Double
    ) : Unit = js.native

    // `fn` should be a function that takes in:
    //   `x` (model parameters)
    //   `grad` (mutate this to return the gradient)
    //   `r`    (mutate this to return the constraint values)
    def addEqualityMConstraint(
      fn: js.Function3[js.Array[Double], js.Array[Double], js.Array[Double], Unit],
      tolerance: js.Array[Double]
    ) : Unit = js.native

  @js.native
  object GC extends js.Object:
    def flush(): Unit = js.native
