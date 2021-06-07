package monet

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

trait Operations[T]:
  def const(c: Double): T
  def zero: T
  def v(n: Int, a: Double): T
  def neg(a: T): T
  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def mult(a: T, b: T): T
  def div(a: T, b: T): T
  def sin(a: T): T
  def cos(a: T): T
  def toInt(a: T): Int

// Here's the problem:
// When we are using our AD DSL, we want to be able to use
// infix operators on doubles and automatically convert them
// into `Diff` nodes if the need arises, like `4.0 * x*x`, or
// `1 + y` -- really, any arithmetic expression with a constant
// on the left.
//
// In a different context, when we are writing _operations_ for
// our AD DSL, we want to be able to write functions that are
// agnostic to whether they are operating on Diff nodes or plain
// doubles, as long as the requisite ops are ther. Along the lines of:
//
// def foo[T: Operations](a: T) =
//   a * a
//
// To achieve this second objective we need to be able to derive syntax
// for any `T`, namely, the generic syntax contained here.
// However, since we have Double as one of our `T`s, then there is
// syntax derived for it, which means that the above operation `1 + y`
// won't convert the Double -- the expression will instead expect `y`
// to be a Double and then error out.
//
// As a result: We keep GenericTSyntax scoped such that we import it
// only in the event we need to support the second context. When it is
// in scope, we cannot use doubles as constants on the left.
//
// An alternative solution: scope `given Operations[Double]` in the
// same way. We elect not to do this because it would require that to
// be in scope anytime the type Pt[Double] (or really the type of any
// container generic over T used with Double) is written, which has
// a far larger surface area.
object GenericTSyntax:
  extension [T](a: T)(using o: Operations[T])
    def +(b: T) = o.add(a,b)
    def +(b: Double) = o.add(a,o.const(b))
    def -(b: T) = o.sub(a,b)
    def -(b: Double) = o.sub(a,o.const(b))
    def *(b: T) = o.mult(a,b)
    def *(b: Double) = o.mult(a,o.const(b))
    def /(b: T) = o.div(a,b)
    def /(b: Double) = o.div(a,o.const(b))
    def unary_- = o.sub(o.zero, a)
    def sin = o.sin(a)
    def cos = o.cos(a)
    def toInt = o.toInt(a)

extension (a: Diff)(using o: Operations[Diff])
  def +(b: Diff) = o.add(a,b)
  def +(b: Double) = o.add(a,o.const(b))
  def -(b: Diff) = o.sub(a,b)
  def -(b: Double) = o.sub(a,o.const(b))
  def *(b: Diff) = o.mult(a,b)
  def *(b: Double) = o.mult(a,o.const(b))
  def /(b: Diff) = o.div(a,b)
  def /(b: Double) = o.div(a,o.const(b))
  def unary_- = o.sub(o.zero, a)
  def sin = o.sin(a)
  def cos = o.cos(a)
  def toInt = o.toInt(a)

extension (a: Double)(using o: Operations[Diff], c : DiffContext)
  def c = o.const(a)
  def v = o.v(c.freshTemp, a)
  def +(b: Diff) = o.add(o.const(a),b)
  def *(b: Diff) = o.mult(o.const(a),b)

// Parametrized over any T that has an operations instance,
// we can create an instance of the given conversion.
given (using o: Operations[Diff]) : Conversion[Double,Diff] with
  def apply(a: Double):Diff = o.const(a)
given (using o: Operations[Diff]) : Conversion[Int,Diff] with
  def apply(a: Int):Diff = o.const(a)

def registerChild(child: Diff, parents: Diff*): Diff =
  for (parent <- parents)
    child.parent(parent)
    parent.read(child)
  child

given (using DiffContext) : Operations[Diff] with
  def const(c: Double) = Const(c)
  def zero: Diff = Const(0)
  def v(n: Int, a: Double) = Var(n,a)
  def neg(a: Diff): Diff = registerChild(Sub(zero,a),a)
  def add(a: Diff, b: Diff): Diff = registerChild(Sum(a,b),a,b)
  def sub(a: Diff, b: Diff): Diff = registerChild(Sub(a,b),a,b)
  def mult(a: Diff, b: Diff): Diff = registerChild(Mul(a,b),a,b)
  def div(a: Diff, b: Diff): Diff = registerChild(Div(a,b),a,b)
  def sin(a: Diff): Diff = registerChild(Sin(a),a)
  def cos(a: Diff): Diff = registerChild(Cos(a),a)
  def toInt(a: Diff): Int = a.primal.toInt

given Operations[Double] with
  def const(c: Double) = c
  def zero: Double = 0
  def v(n: Int, a: Double) = a
  def neg(a: Double): Double = -a
  def add(a: Double, b: Double): Double = a+b
  def sub(a: Double, b: Double): Double = a-b
  def mult(a: Double, b: Double): Double = a*b
  def div(a: Double, b: Double): Double = a/b
  def sin(a: Double): Double = math.sin(a)
  def cos(a: Double): Double = math.cos(a)
  def toInt(a: Double): Int = a.toInt


// Plumbing (Key into a hashmap using an arbitrary order of multiple keys)
class CacheKey(val keys: Seq[Diff]):
  // Todo (performance): make this work for keys in any order
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[CacheKey])
      val otherKey = obj.asInstanceOf[CacheKey]
      (otherKey.keys.size == keys.size) &&
      !otherKey.keys.zip(keys).exists((a,b) => a != b)
    else
      false

  // This already works for keys in any order
  override def hashCode(): Int =
    var result = 0
    for (key <- keys)
      result += key.hashCode()
    result

class DiffContext:
  var currentID = 0
  def freshTemp =
    currentID += 1
    currentID

  // Gets the derivative of the parameters with respect to the target nodes.
  def d(parameters: Seq[Diff], targets: Seq[Diff], step: Double = 1.0) =
    val stack = Stack[Diff]()
    val seen = Set[Diff]()

    def register(node: Diff, value : Double = 0.0) =
      if (!(seen contains node))
        node.gradient = value
        seen.add(node)
      stack.push(node)

    def visit(target: Diff) =
      register(target, step)
      while (stack.length != 0)
        var node = stack.pop()
        node._visits_this_evaluation += 1
        if (node._current_uses <= node._visits_this_evaluation)
          node._visits_this_evaluation = 0
          for (parent <- node.parents)
            register(parent)
          node.reverseDerivative(node.gradient)

    targets.map(visit)
    parameters.map(_.gradient)

  // The generation mechanism keeps us from performing excess recomputation
  // for nodes that are not used in the gradient evaluation.
  var currentGeneration = 0

  // In the current computation graph, compute the number of uses of each
  // node that go into computing a given set of targets. This automatically
  // filters out uses that don't end up being used, since their adjoint will
  // not be propagated up.
  def prepare(targets: Seq[Diff]): Unit =
    topologicalCache.clear
    currentGeneration += 1
    val rooted = Set.from(targets)
    val nextList = Stack.from(rooted)

    while (nextList.length > 0)
      val current = nextList.pop()
      for (parent <- current.parents)
        if (!(rooted contains parent))
          rooted.add(parent)
          nextList.push(parent)

    for (current <- rooted)
      current._current_uses = current.uses.count(n => rooted contains n)
      current._generation = currentGeneration

  var topologicalCache = scala.collection.mutable.Map[CacheKey, Seq[Diff]]()

  def recompute(order : Seq[Diff], seen: Set[Diff]) =
    for (node <- order if !(seen contains node))
      node.recompute

  // def update(mapping: (Dif))
  import scala.scalajs.js

  def update(parameters: Seq[Diff], newValues: js.Array[Double]): Unit =
    for ((node,newValue) <- parameters.zip(newValues))
      node.primal = newValue
    val updatedNodes = parameters
    val seen = Set.from(updatedNodes)

    topologicalCache.get(CacheKey(updatedNodes)) match
      case Some(ordering) => recompute(ordering, seen)
      case None =>
        val order = ArrayBuffer[Diff]()
        val permanent = Set[Diff]()
        val temporary = Set[Diff]()

        def visit(node: Diff) : Unit =
          if (!node.isRooted) return
          if (permanent contains node) return
          if (temporary contains node) throw new IllegalArgumentException("Cyclical")
          temporary.add(node)
          node.uses.map(visit)
          temporary.remove(node)
          permanent.add(node)
          order.append(node)

        seen.map(visit)
        val recomputeOrder = order.reverse.toSeq
        topologicalCache.put(CacheKey(updatedNodes),recomputeOrder)
        recompute(recomputeOrder, seen)

  def update(mapping: (Diff,Double)*) : Unit =
    import js.JSConverters._
    val (params, values) = mapping.unzip
    update(params, values.toJSArray)

class Diff (direct: => Double)(using ctx: DiffContext) { self =>
  val reverseDerivative: Double => Unit = (d: Double) => ()
  var primal : Double = direct
  var partial = 0.0
  var gradient = 0.0
  val uses = ArrayBuffer[Diff]()
  val parents = ArrayBuffer[Diff]()

  var _generation = -1
  var _current_uses = 0
  var _visits_this_evaluation = 0

  def recompute =
    primal = direct
  def read(dependent: Diff) =
    uses += dependent
  def parent(p : Diff) =
    parents += p
  def isRooted : Boolean =
    _generation == ctx.currentGeneration

  def d(parameters: Seq[Diff], step: Double = 1.0) =
    ctx.d(parameters, Seq(self), step)

  lazy val parameters : Set[Diff] =
    parents.foldLeft(Set.empty)((result,p) => result ++ p.parameters)
}

def Const(value: Double)(using ctx: DiffContext) : Diff =
  new Diff(value) {
    override lazy val parameters : Set[Diff] = Set()
  }

def Var(id: Int, value: Double)(using ctx: DiffContext) : Diff =
  new Diff(value) { self =>
    override val reverseDerivative = (d: Double) =>
      self.gradient += d
    override lazy val parameters : Set[Diff] = Set(self)
  }

def Sum(a : Diff, b: Diff)
  (using o: Operations[Diff], ctx: DiffContext): Diff =
  new Diff(a.primal + b.primal) {
    override val reverseDerivative = (d: Double) =>
      a.gradient += d
      b.gradient += d
  }

def Mul(a : Diff, b: Diff)
  (using o: Operations[Diff], ctx: DiffContext): Diff =
  new Diff(a.primal * b.primal) {
    override val reverseDerivative = (d: Double) =>
      a.gradient += d * b.primal
      b.gradient += a.primal * d
  }

def Sub(a : Diff, b: Diff)
  (using o: Operations[Diff], ctx: DiffContext): Diff =
  new Diff(a.primal - b.primal) {
    override val reverseDerivative = (d: Double) =>
      a.gradient += d
      b.gradient -= d
  }

def Div(a : Diff, b: Diff)
  (using o: Operations[Diff], ctx: DiffContext): Diff =
  new Diff(a.primal / b.primal) {
    override val reverseDerivative = (d: Double) =>
      val gx2 = b.primal * b.primal
      a.gradient += (b.primal * d) / gx2
      b.gradient -= (a.primal * d) / gx2
  }

def Sin(a : Diff)
  (using o: Operations[Diff], ctx: DiffContext): Diff =
  new Diff(math.sin(a.primal)) {
    override val reverseDerivative = (d: Double) =>
      a.gradient += d * math.cos(d.primal)
  }

def Cos(a : Diff)
  (using o: Operations[Diff], ctx: DiffContext): Diff =
  new Diff(math.cos(a.primal)) {
    override val reverseDerivative = (d: Double) =>
      a.gradient -= d * math.sin(d.primal)
  }


def parameters(ps: Seq[Double])(using DiffContext): Seq[Diff] =
  ps.map(p => p.v)

object TestSemantics:
  def test =
    given DiffContext = new DiffContext()
    val Seq(a) = parameters { Seq(2) }
    val b = 3
    val c  = (4.0*a*a)+b*b
    println(c.d(Seq(a),1.0))