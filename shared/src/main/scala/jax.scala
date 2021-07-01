package jax

import java.rmi.server.Operation

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

// Syntactic sugar
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

// Semantics offers an interface that, with access to operations for an underlying type,
// can provide different ways of interpreting this type in a wrapper. For example, we
// can wrap with `Diff`, etc. This corresponds to a MainTrace in JAX, and W[_] corresponds
// to the Tracer type for the given transform.
abstract class Semantics[W[_],T](using _ops: Operations[T]) extends Operations[W[T]]:
  val ops = _ops
  def const(a: Double) = lift(ops.const(a))
  def lift(a: T): W[T]
  def lower(a: W[T]): T

// Nothing but a box
case class Direct[T](val value: T)

class DirectSemantics[T](using ops: Operations[T]) extends Semantics[Direct,T]:
  def lift(a: T) : Direct[T] = Direct(a)
  def lower(a: Direct[T]) : T = a.value
  def add(a: Direct[T], b: Direct[T]) = Direct(a.value + b.value)
  def mul(a: Direct[T], b: Direct[T]) = Direct(a.value * b.value)
  def neg(a: Direct[T]) : Direct[T] = Direct(ops.neg(a.value))
  def sin(a: Direct[T]) : Direct[T] = Direct(ops.sin(a.value))
  def cos(a: Direct[T]) : Direct[T] = Direct(ops.cos(a.value))

given Operations[Double] with
  def const(a: Double) = a
  def add(a: Double, b: Double) = a + b
  def mul(a: Double, b: Double) = a * b
  def neg(a: Double) = -a
  def sin(a: Double) = math.sin(a)
  def cos(a: Double) = math.cos(a)

// A box, but now with dual numbers
case class JVP[T](val value: T, val tangent: T)

class JVPSemantics[T](using ops: Operations[T]) extends Semantics[JVP, T]:
  def lift(a: T) : JVP[T] = JVP(a,ops.const(0))
  def lower(a: JVP[T]) : T = a.value
  def add(a: JVP[T], b: JVP[T]) =
    val fwd = a.value + b.value
    val jvp = a.tangent + b.tangent
    JVP(fwd, jvp)
  def mul(a: JVP[T], b: JVP[T]) =
    val fwd = a.value * b.value
    val jvp = (a.tangent * b.value) + (a.value * b.tangent)
    JVP(fwd,jvp)
  def neg(a: JVP[T]) : JVP[T] = JVP(- a.value, -a.tangent)
  def sin(a: JVP[T]) : JVP[T] = JVP(ops.sin(a.value), ops.cos(a.value) * a.tangent)
  def cos(a: JVP[T]) : JVP[T] = JVP(ops.cos(a.value),-(ops.sin(a.value)) * a.tangent)


def direct[T](f: T => T, arg: T)(using Operations[T]) =
  f(arg)

// Basically, we need to be able to take in `f` which is a value that takes in any type
// that has `Operations` and returns _that same type_. Of course we want that type `A`
// in this case to be `JVP[T]`. We model this by mixing Scala 3's features:
//  - Polymorphic function types [https://dotty.epfl.ch/docs/reference/new-types/polymorphic-function-types.html]
//  - Context Functions [https://dotty.epfl.ch/docs/reference/contextual/context-functions.html]
def jvp[T](f: [A] => (Operations[A]) ?=> A => A, arg: T, tangent: T)(using Operations[T]) : JVP[T] =
  // Set up a new context
  given semantics : JVPSemantics[T] = new JVPSemantics[T]
  // Lift all of the values to JVP
  val inputs = JVP(arg,tangent)

  f[JVP[T]](inputs)

// Stage a lambda such that this directly performs the derivative computation. The critical
// thing here is that we _do not know the type `A`_. `f` must be a polymorphic method, since
// we want to be able to take the derivative both of, say, `Double => Double` and `JVP[Double] => JVP[Double]`.
// We cannot concretize such a type -- if we did, the type of `f` would be JVP[T] for some concrete `T`,
// and then the _output_ of the deriv function (if it returned T => T) would _no longer be generic_,
// since we'd have concretized it to whatever T was at the call site. See `graveyard.scala` for other examples.
def deriv(f: [A] => (Operations[A]) ?=> A => A) : [A] => (Operations[A]) ?=> A => A =
  [A] => (ops: Operations[A]) ?=>
    (x: A) =>
      jvp(f,x,ops.const(1)).tangent


// We have the following function that we want to transform. This
// function is abstract, independent of any context, and basically
// outlines what shold happen to whatever tracer value we plug in.
def foo[T : Operations](x:T) =
  val z = x + x
  z

// Here's the function from the JAX code:
def bar[T: Operations](x: T) =
  val y = sin(x) * 2
  val z = - y + x
  z

object Main extends App:
  val f = [T] => (ops: Operations[T]) ?=>
    (x: T) =>
      foo[T](x)         // f(x) = 2x

  val df  = deriv(f)
  println(df(3.0))      // 2.0 (slope)
  val d2f = deriv(deriv(f))
  println(d2f(3.0))     // 0.0 (second derivative is constant)
  println("\n")


  // And look, it works even for their test function!
  val g = [T] => (ops: Operations[T]) ?=>
    (x: T) =>
      bar[T](x)         // f(x) = -2sin(x) + x

  println(deriv(g)(3.0))
  println(deriv(deriv(g))(3.0))
  println(deriv(deriv(deriv(g)))(3.0))
  println(deriv(deriv(deriv(deriv(g))))(3.0))


  // Use the same example as in the JAX docs
  println("\n")
  println("Checking sin")

  val s = [T] => (ops: Operations[T]) ?=>
    (x: T) =>
      sin(x)
  println(deriv(s)(3.0))
  println(deriv(deriv(s))(3.0))
  println(deriv(deriv(deriv(s)))(3.0))
  println(deriv(deriv(deriv(deriv(s))))(3.0))
