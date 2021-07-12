package jax

// This is an attempt to mimic the JAX architecture as described in
// https://jax.readthedocs.io/en/latest/autodidax.html, the main difference
// being that we have to figure out what types to give Scala to construct
// the appropriate transformations, and we can rely a lot more on context
// functions and implicits to source our semantics rather than an explicit
// stack in the MainTrace referenced by Trace objects.

// Operations provide the available list of things we know how to do to
// any given datatype, whether it be a double, an array, a diff object, etc.
trait Operations[T] {
  def const(a: Double): T
  def add(a: T, b: T): T
  def mul(a: T, b: T): T
  def neg(a: T): T
  def sin(a: T): T
  def cos(a: T): T
}

// Syntactic sugar.
// `extension` is Scala 3's version of `implicit class`, and seems to work in more cases.
// https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html
// `using` denotes implicit parameters as arguments.
// https://dotty.epfl.ch/docs/reference/contextual/using-clauses.html
extension [T](a: T)(using ops: Operations[T])
  def +(b: T) = ops.add(a, b)
  def *(b: T) = ops.mul(a, b)
  def unary_- = ops.neg(a)

def sin[T](a: T)(using ops: Operations[T]) = ops.sin(a)
def cos[T](a: T)(using ops: Operations[T]) = ops.cos(a)

// Allow infix ops with doubles to automatically perform const-conversion
extension [T](a: T)(using ops: Operations[T])
  def +(b: Double) = ops.add(a, ops.const(b))
  def *(b: Double) = ops.mul(a, ops.const(b))

// We provide a root operations instance that tells us we can work with Doubles.
// `givens` are the same as the old `implicit val =`
// https://dotty.epfl.ch/docs/reference/contextual/givens.html
given Operations[Double] with
  def const(a: Double) = a
  def add(a: Double, b: Double) = a + b
  def mul(a: Double, b: Double) = a * b
  def neg(a: Double) = -a
  def sin(a: Double) = math.sin(a)
  def cos(a: Double) = math.cos(a)

// Semantics offers an interface that, with access to operations for an underlying type,
// can provide different ways of interpreting this type in a wrapper. For example, we
// can wrap with `Diff`, etc. This corresponds to a MainTrace in JAX, and W[_] corresponds
// to the Tracer type for the given transform.
abstract class Semantics[W[_], T](using _ops: Operations[T]) extends Operations[W[T]]:
  val ops = _ops
  def const(a: Double) = lift(ops.const(a))
  def lift(a: T): W[T]

// Use a box to hold a dual value, not just singular Doubles.
case class JVP[T](val value: T, val tangent: T)

// Semantics for our dual numbers. This is forward-mode autodiff (Jacobian-Vector Product).
class JVPSemantics[T](using ops: Operations[T]) extends Semantics[JVP, T]:
  def lift(a: T): JVP[T] = JVP(a, ops.const(0))
  def add(a: JVP[T], b: JVP[T]) =
    val fwd = a.value + b.value
    val jvp = a.tangent + b.tangent
    JVP(fwd, jvp)
  def mul(a: JVP[T], b: JVP[T]) =
    val fwd = a.value * b.value
    val jvp = (a.tangent * b.value) + (a.value * b.tangent)
    JVP(fwd, jvp)
  def neg(a: JVP[T]): JVP[T] = JVP(-a.value, -a.tangent)
  def sin(a: JVP[T]): JVP[T] = JVP(ops.sin(a.value), ops.cos(a.value) * a.tangent)
  def cos(a: JVP[T]): JVP[T] = JVP(ops.cos(a.value), -(ops.sin(a.value)) * a.tangent)

// Basically, we need to be able to take in `f` which is a value that takes in any type
// that has `Operations` and returns _that same type_. Of course we want that type `A`
// in this case to be `JVP[T]`. We model this by mixing Scala 3's features:
//  - Polymorphic function types [https://dotty.epfl.ch/docs/reference/new-types/polymorphic-function-types.html]
//  - Context Functions [https://dotty.epfl.ch/docs/reference/contextual/context-functions.html]
def jvp[T](f: [A] => (Operations[A]) ?=> A => A, arg: T, tangent: T)(using Operations[T]): JVP[T] =
  // Set up a new context
  given semantics: JVPSemantics[T] = new JVPSemantics[T]
  // Lift all of the values to JVP
  val inputs = JVP(arg, tangent)

  f[JVP[T]](inputs)

// Stage a lambda such that this directly performs the derivative computation. The critical
// thing here is that we _do not know the type `A`_. `f` must be a polymorphic method, since
// we want to be able to take the derivative both of, say, `Double => Double` and `JVP[Double] => JVP[Double]`.
// We cannot concretize such a type -- if we did, the type of `f` would be JVP[T] for some concrete `T`,
// and then the _output_ of the deriv function (if it returned T => T) would _no longer be generic_,
// since we'd have concretized it to whatever T was at the call site. See `graveyard.scala` for other examples.
def deriv(f: [A] => (Operations[A]) ?=> A => A): [A] => (Operations[A]) ?=> A => A =
  [A] => (ops: Operations[A]) ?=> (x: A) => jvp(f, x, ops.const(1)).tangent

// We have the following function that we want to transform. This
// function is abstract, independent of any context, and basically
// outlines what shold happen to whatever tracer value we plug in.
def foo[T: Operations](x: T) =
  val z = x + x
  z

// Here's the function from the JAX code:
def bar[T: Operations](x: T) =
  val y = sin(x) * 2
  val z = -y + x
  z

// Partial evaluation. This trait represents a value that could either be:
// - known (Constant, i.e. evaluated)
// - unknown (Abstract, i.e. staged).
sealed trait Partial[T]:
  def dependencies: Seq[Partial[T]]

class Constant[T](val value: T) extends Partial[T]:
  def dependencies = Seq()

// When staging, we keep track of the dependencies to be able to extract an execution order,
// and the operation type to be able to correctly apply a transpose later.
sealed trait Abstract[T] extends Partial[T]
case class AbstractVariable[T]() extends Abstract[T]:
  def dependencies = Seq()
case class AbstractAdd[T](a: Partial[T], b: Partial[T]) extends Abstract[T]:
  def dependencies = Seq(a, b)
case class AbstractMul[T](a: Partial[T], b: Partial[T]) extends Abstract[T]:
  def dependencies = Seq(a, b)
case class AbstractNeg[T](a: Partial[T]) extends Abstract[T]:
  def dependencies = Seq(a)
case class AbstractSin[T](a: Partial[T]) extends Abstract[T]:
  def dependencies = Seq(a)
case class AbstractCos[T](a: Partial[T]) extends Abstract[T]:
  def dependencies = Seq(a)

// Partially evaluate if all of the inputs are known, otherwise build up the computation graph
class PartialSemantics[T](using ops: Operations[T]) extends Semantics[Partial, T]:
  def match2(a: Partial[T], b: Partial[T], f: (T, T) => T, g: (Partial[T], Partial[T]) => Partial[T]): Partial[T] =
    (a, b) match
      case (ac: Constant[T], bc: Constant[T]) => Constant(f(ac.value, bc.value))
      case _                                  => g(a, b)
  def match1(a: Partial[T], f: T => T, g: Partial[T] => Partial[T]): Partial[T] = a match
    case c: Constant[T] => Constant(f(c.value))
    case _              => g(a)

  def empty(): Partial[T] = AbstractVariable[T]()
  def lift(a: T): Partial[T] = Constant(a)
  def add(a: Partial[T], b: Partial[T]) = match2(a, b, _ + _, AbstractAdd.apply)
  def mul(a: Partial[T], b: Partial[T]) = match2(a, b, _ * _, AbstractMul.apply)
  def neg(a: Partial[T]): Partial[T] = match1(a, -_, AbstractNeg.apply)
  def sin(a: Partial[T]): Partial[T] = match1(a, ops.sin(_), AbstractSin.apply)
  def cos(a: Partial[T]): Partial[T] = match1(a, ops.cos(_), AbstractCos.apply)

  // This is a bit repetitive with the abstract construction -- should instead when constructing abstract over the arity
  // and just pass a function T => T instead. We'll wait until `transpose()` is defined to do this though/
  // ALSO: this does extra work if we don't memoize and `a,b` share subtrees. We should instead serialize.
  def evaluatePartial(partial: Partial[T])(using context: AbstractVariable[T] => T): T = partial match
    case constant: Constant[T]  => constant.value
    case v: AbstractVariable[T] => context(v)
    case AbstractAdd(a, b)      => evaluatePartial(a) + evaluatePartial(b)
    case AbstractMul(a, b)      => evaluatePartial(a) * evaluatePartial(b)
    case AbstractNeg(a)         => -evaluatePartial(a)
    case AbstractSin(a)         => ops.sin(evaluatePartial(a))
    case AbstractCos(a)         => ops.cos(evaluatePartial(a))

  // NB: Right now specialized to functions of one argument and one result.
  // To expand this to vector-valued fucntions we would just have sequences (and a corresponding
  // sequence of tangents of equivalent length to result).
  def evaluateTransposed(parameter: Partial[T], result: Partial[T], tangent: T): T =
    // - A map of cotangent values for each `partial` linear object.
    //   In the case of VJP, these objects limited to `add`, `mul`, and `neg`,
    //   since those are the only operations that JVP uses on *tangent* values -- everything
    //   else actually got partially-evaluated away, since it was only on primals.
    val tangentMap = scala.collection.mutable.Map[Partial[T], T]() // Accumulated delta

    //  Serialize all of the nodes so that each runs only once. Each node will compute its transpose
    //  rule, and modify the cotangents by accumulating into them. Each node will run only once
    //  all of its input cotangents (`d`, in our old system) are accumulated. As in our system, this
    //  can probably be cached.
    val executionOrder: Seq[Partial[T]] = dfs(result)

    // Equivalently we can initialize the tangents for all nodes to zero
    def read_cotangent(p: Partial[T]): T =
      tangentMap.remove(p) match
        case Some(value) => value
        case None        => ops.const(0.0)

    def write_cotangent(p: Partial[T], delta: T): Unit =
      tangentMap.get(p) match
        case Some(accumulated) => tangentMap(p) = accumulated + delta
        case None              => tangentMap(p) = delta

    // Each transpose operates directly on cotangents and abstract values. One
    // notable difference between this transformation and our previous RAD method
    // is that the rules are _significantly_ simpler, and much of the computation
    // has been evaluated away in `linearize`.
    def transpose(partial: Partial[T]) =
      partial match
        // These are no-ops
        case c: Constant[T]         => ()
        case v: AbstractVariable[T] => ()

        case AbstractAdd(a, b) =>
          val z_bar = read_cotangent(partial)
          write_cotangent(a, z_bar)
          write_cotangent(b, z_bar)

        case AbstractMul(a, b) =>
          val z_bar = read_cotangent(partial)
          a match
            case cA: Constant[T] => write_cotangent(b, cA.value * z_bar)
            case _ =>
              b match
                case cB: Constant[T] => write_cotangent(a, z_bar * cB.value)
                case _               => throw new Exception("non-linear function")
        // Can't have both be abstract.

        case AbstractNeg(a) =>
          val y_bar = read_cotangent(partial)
          write_cotangent(a, -y_bar)

        case _ =>
          throw new Exception(s"Operation $partial is nonlinear")

    write_cotangent(result, tangent)
    for (operation <- executionOrder.reverse)
      transpose(operation)
    read_cotangent(parameter)

def linearize[T](f: [A] => (Operations[A]) ?=> A => A, x: T)(using Operations[T]): (T, T => T) =
  // Set up a new context
  given partial: PartialSemantics[T] = new PartialSemantics[T]
  given jvp: JVPSemantics[Partial[T]] = new JVPSemantics[Partial[T]]

  // take the JVP of the function, but leave the tangents abstract.
  val inputs = JVP[Partial[T]](partial.lift(x), partial.empty())
  val linear = f[JVP[Partial[T]]](inputs)

  val result: T = linear.value match
    case c: Constant[T] => c.value
    case _              => throw new Error("Primal value depends on tangents")

  val f_lin = (tangent: T) =>
    given ctx: (AbstractVariable[T] => T) = _ => tangent // Context has only one variable
    partial.evaluatePartial(linear.tangent)(using ctx)

  (result, f_lin)

var PRINT = true

def vjp[T](f: [A] => (Operations[A]) ?=> A => A, x: T)(using Operations[T]): T => T =
  // Very, very similar to linearize -- all of the setup is the same. The key difference
  // in the lambda we return: in `linearize`, we evaluate the tangent directly (JVP).
  // Here, we evaluate the tangent with the custom transpose rules.
  given partial: PartialSemantics[T] = new PartialSemantics[T]
  given jvp: JVPSemantics[Partial[T]] = new JVPSemantics[Partial[T]]

  val tangentParameter = partial.empty()
  val inputs = JVP[Partial[T]](partial.lift(x), tangentParameter)
  val linear = f[JVP[Partial[T]]](inputs)

  (tangent: T) =>
    if (PRINT)
      printAbstractProgram(linear.tangent)
    partial.evaluateTransposed(tangentParameter, linear.tangent, tangent)

def grad(f: [A] => (Operations[A]) ?=> A => A) =
  [A] => (ops: Operations[A]) ?=> (x: A) => vjp(f, x)(ops.const(1.0))

//
//--------------------- HELPER FUNCTIONS ---------------------
//

// extract a topological sort
def dfs[T](op: Partial[T]): Seq[Partial[T]] =
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable.Set

  val order = ArrayBuffer[Partial[T]]()
  val seen = Set[Partial[T]](op)
  val temporary = Set[Partial[T]]()
  val permanent = Set[Partial[T]]()

  def visit(current: Partial[T]): Unit =
    if (permanent contains current) return
    if (temporary contains current) throw new Exception("Cyclic dependency")
    temporary += (current)
    for (dep <- current.dependencies)
      visit(dep)
    temporary -= (current)
    permanent += (current)
    order += current

  for (origin <- seen)
    visit(origin)

  order.toSeq

def printAbstractProgram[T](partial: Partial[T]): Unit =
  import scala.collection.mutable.Map
  val id = Map[Partial[T], String]()
  val counter = Map[String, Int]("t" -> 0, "v" -> 0)
  def definePrefix(prefix: String)(p: Partial[T]): String =
    var newName = p match
      case c: Constant[T] => s"${c.value}"
      case _ =>
        counter(prefix) += 1
        s"$prefix${counter(prefix)}"
    id(p) = newName
    newName
  def define(p: Partial[T]) = definePrefix("t")(p)
  def name(p: Partial[T]): String =
    id.get(p) match
      case Some(s) => s
      case None    => definePrefix("v")(p)

  val order = dfs(partial)

  val exprs = order.map(op =>
    op match
      case c: Constant[T]         => ""
      case v: AbstractVariable[T] => ""
      case AbstractAdd(a, b)      => s"${define(op)} = ${name(a)} + ${name(b)}"
      case AbstractMul(a, b)      => s"${define(op)} = ${name(a)} * ${name(b)}"
      case AbstractNeg(a)         => s"${define(op)} = -${name(a)}"
      case AbstractSin(a)         => s"${define(op)} = sin(${name(a)})"
      case AbstractCos(a)         => s"${define(op)} = cos(${name(a)})"
  )

  println(exprs.filter(_.size > 0).mkString("\n"))
