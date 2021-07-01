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

  // var f_evals = 0
  // var g_evals = 0

  opt.setMinObjective((x, grad) =>
    val result = f(x)
    // f_evals += 1
    if (grad != null)
      val _grad = g(x)
      for ((g,i) <- _grad.zipWithIndex)
        grad(i) = g
      // g_evals += 1
    result
  , 1e-4)

  val res = opt.optimize(initial.toJSArray)
  // println(s"Optimized in $f_evals iterations ($g_evals gradient iters) [${t1-t0}ms]")
  nlopt.GC.flush()
  res.x

// Nicer API for the above function that specializes diffs
def optimize(
  parameters: Seq[Diff],
  objective: Diff
)(using ctx: DiffContext): Seq[Double] =
  optimize(
      parameters.map(_.primal),
      (newParams) => { ctx.update(parameters, newParams); objective.primal },
      (newParams) => { ctx.update(parameters, newParams); objective.d(parameters, 1.0) }
  ).toSeq

def optimizeWithEq(
  initial: Seq[Double],
  f: js.Array[Double] => Double,
  g: js.Array[Double] => Seq[Double],
  equalities: js.Array[Double] => Seq[Double],
  equalityGradients: js.Array[Double] => Seq[Seq[Double]],
  m: Int
): js.Array[Double] =
  val tolerance = 1e-3
  val opt = new nlopt.Optimize(nlopt.Algorithm.LD_SLSQP, initial.length)

  opt.setMinObjective((x, grad) =>
    val result = f(x)
    if (grad != null)
      val _grad = g(x)
      for ((g,i) <- _grad.zipWithIndex)
        grad(i) = g
    result
  , tolerance)

  val tolerances = (0 until m).map(_ => tolerance).toJSArray
  opt.addEqualityMConstraint((x,grad,r) =>
    val eqs = equalities(x)
    val n = eqs.size
    if (grad != null)
      val _grads = equalityGradients(x)
      var offset = 0
      for (_grad <- _grads)
        for ((g,i) <- _grad.zipWithIndex)
          grad(offset+i) = g
        offset += n
    for ((value,i) <- eqs.zipWithIndex)
      r(i) = value
  , tolerances)

  val res = opt.optimize(initial.toJSArray)
  nlopt.GC.flush()
  res.x

def optimizeIndividualEq(
  initial: Seq[Double],
  f: js.Array[Double] => Double,
  g: js.Array[Double] => Seq[Double],
  equalities: Seq[js.Array[Double] => Double],
  equalityGradients: Seq[js.Array[Double] => Seq[Double]],
) =
  val tolerance = 1e-3
  val opt = new nlopt.Optimize(nlopt.Algorithm.LD_SLSQP, initial.length)

  opt.setMinObjective((x, grad) =>
    val result = f(x)
    if (grad != null)
      val _grad = g(x)
      for ((g,i) <- _grad.zipWithIndex)
        grad(i) = g
    result
  , tolerance)

  for ((eq,eq_g) <- equalities.zip(equalityGradients))
    opt.addEqualityConstraint((x,grad) =>
      val result = eq(x)
      if (grad != null)
        val _grad = eq_g(x)
        for ((g,i) <- _grad.zipWithIndex)
          grad(i) = g
      result,
      tolerance)

  val res = opt.optimize(initial.toJSArray)
  nlopt.GC.flush()
  res.x

def optimizeEqM(
  parameters: Seq[Diff],
  objective: Diff,
  constraints: Seq[Diff]
)(using ctx: DiffContext) : Seq[Double] =
  optimizeWithEq(
      parameters.map(_.primal),
      (newParams) => { ctx.prepare(Seq(objective)); ctx.update(parameters, newParams); objective.primal },
      (newParams) => { ctx.update(parameters, newParams); objective.d(parameters, 1.0) },
      (newParams) =>
          ctx.prepare(constraints)
          ctx.update(parameters, newParams)
          constraints.map(_.primal),
      (newParams) =>
        constraints.map(c =>
          ctx.prepare(Seq(c))
          ctx.update(parameters, newParams)
          c.d(parameters, 1.0)
        ),
      constraints.length
  ).toSeq

def optimizeEq(
  parameters: Seq[Diff],
  objective: Diff,
  constraints: Seq[Diff]
)(using ctx: DiffContext) : Seq[Double] =
  optimizeIndividualEq(
      parameters.map(_.primal),
      (newParams) => { ctx.prepare(Seq(objective)); ctx.update(parameters, newParams); objective.primal },
      (newParams) => { ctx.update(parameters, newParams); objective.d(parameters, 1.0) },
      constraints.map(c =>
        (newParams) =>
          ctx.prepare(Seq(c))
          ctx.update(parameters, newParams)
          c.primal
      ),
      constraints.map(c =>
        (newParams) =>
          ctx.prepare(Seq(c))
          ctx.update(parameters, newParams)
          c.d(parameters, 1.0)
      )
  ).toSeq