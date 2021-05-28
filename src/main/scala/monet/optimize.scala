package monet

import scala.scalajs.js
import typings.nlopt._
import js.JSConverters._

// import org.scalajs.dom.raw.Performance

import js.annotation._

@JSGlobal
@js.native
object performance extends js.Object {
  def now() : Int = js.native
}

def optimize(
  initial: Seq[Double],
  f: js.Array[Double] => Double,
  g: js.Array[Double] => Seq[Double]
): js.Array[Double] =
  val opt = new nlopt.Optimize(nlopt.Algorithm.LD_SLSQP, initial.length)

  var f_evals = 0
  var g_evals = 0

  opt.setMinObjective((x, grad) =>
    val result = f(x)
    f_evals += 1
    if (grad != null)
      val _grad = g(x)
      for ((g,i) <- _grad.zipWithIndex)
        grad(i) = g
      g_evals += 1
    result
  , 1e-3)

  val t0 = performance.now()
  val res = opt.optimize(initial.toJSArray)
  val t1 = performance.now()
  // println(s"Optimized in $f_evals iterations ($g_evals gradient iters) [${t1-t0}ms]")
  nlopt.GC.flush()
  res.x

