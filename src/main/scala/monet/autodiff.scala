package monet

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

trait Operations[T]:
  def const(c: Double): T
  def zero: T
  def lower(t : T) : Double
  def v(n: Int, a: Double): T
  def neg(a: T): T
  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def mult(a: T, b: T): T
  def div(a: T, b: T): T
  def sin(a: T): T
  def cos(a: T): T

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

extension [T](a: Double)(using o: Operations[T], c : DiffContext)
  def v = o.v(c.freshTemp, a)
  def +(b: T) = o.add(o.const(a),b)
  def *(b: T) = o.mult(o.const(a),b)

// Parametrized over any T that has an operations instance,
// we can create an instance of the given conversion.
given [T](using o: Operations[T]) : Conversion[Double,T] with
  def apply(a: Double):T = o.const(a)
given [T](using o: Operations[T]) : Conversion[Int,T] with
  def apply(a: Int):T = o.const(a)

def registerChild(child: Diff, parents: Diff*): Diff =
  for (parent <- parents)
    child.parent(parent)
    parent.read(child)
  child

given (using DiffContext) : Operations[Diff] with
  def const(c: Double) = Diff(c)
  def zero: Diff = const(0)
  def v(n: Int, a: Double) = Var(n,a)
  def lower(t: Diff) = t.primal

  def neg(a: Diff): Diff = registerChild(Sub(zero,a),a)
  def add(a: Diff, b: Diff): Diff = registerChild(Sum(a,b),a,b)
  def sub(a: Diff, b: Diff): Diff = registerChild(Sub(a,b),a,b)
  def mult(a: Diff, b: Diff): Diff = registerChild(Mul(a,b),a,b)
  def div(a: Diff, b: Diff): Diff = registerChild(Div(a,b),a,b)
  def sin(a: Diff): Diff = registerChild(Sin(a),a)
  def cos(a: Diff): Diff = registerChild(Cos(a),a)


// given Operations[Double] with
//   def const(c: Double) = c
//   def zero: Double = const(0)
//   def v(n: Int, a: Double) = a
//   def lower(t: Double) = t
//   def neg(a: Double): Double = -a
//   def add(a: Double, b: Double): Double = a+b
//   def sub(a: Double, b: Double): Double = a-b
//   def mult(a: Double, b: Double): Double = a*b
//   def div(a: Double, b: Double): Double = a*b


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

  // In the current computation graph, compute the number of uses of each
  // node that go into computing a given set of targets. This automatically
  // filters out uses that don't end up being used, since their adjoint will
  // not be propagated up.
  def prepare(targets: Seq[Diff]): Unit =
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

  def update(mapping: (Diff,Double)*): Unit =
    for ((node,newValue) <- mapping)
      node.primal = newValue
    val updatedNodes = mapping.map(_._1)
    val seen = Set.from(updatedNodes)

    // Todo: path cache
    val order = ArrayBuffer[Diff]()
    val permanent = Set[Diff]()
    val temporary = Set[Diff]()

    def visit(node: Diff) : Unit =
      if (permanent contains node) return
      if (temporary contains node) throw new IllegalArgumentException("Cyclical")
      temporary.add(node)
      node.uses.map(visit)
      temporary.remove(node)
      permanent.add(node)
      order.append(node)

    seen.map(visit)
    for (node <- order.reverse)
      if (!(seen contains node)) node.recompute


class Diff (direct: => Double)(using ctx: DiffContext) { self =>
  val reverseDerivative: Double => Unit = (d: Double) => ()
  var primal : Double = direct
  var partial = 0.0
  var gradient = 0.0
  val uses = ArrayBuffer[Diff]()
  val parents = ArrayBuffer[Diff]()
  var _current_uses = 0
  var _visits_this_evaluation = 0

  def recompute =
    primal = direct
  def read(dependent: Diff) =
    uses += dependent
  def parent(p : Diff) =
    parents += p

  def d(parameters: Seq[Diff], step: Double = 1.0) =
    ctx.d(parameters, Seq(self), step)
}

def Var(id: Int, value: Double)(using ctx: DiffContext) : Diff =
  new Diff(value) { self =>
    override val reverseDerivative = (d: Double) =>
      self.gradient += d
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


def parameters(ps: Seq[Double])(using DiffContext) =
  ps.map(p => p.v)

object TestSemantics:
  def test =
    given DiffContext = new DiffContext()
    val Seq(a) = parameters { Seq(2) }
    val b = 3
    val c = 4*a*a+b*b
    println(c.d(Seq(a),1.0))