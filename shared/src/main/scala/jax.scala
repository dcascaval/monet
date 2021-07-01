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
  // def mul(a: T, b: T): W[T]
  // def sin(a: T): T
  // def cos(a: T): T
}

// Semantics offers an interface that, with access to operations for an underlying type,
// can provide different ways of interpreting this type in a wrapper. For example, we
// can wrap with `Diff`, etc. This corresponds to a MainTrace in JAX, and W[_] corresponds
// to the Tracer type for the given transform.
abstract class Semantics[W[_],T](using _ops: Operations[T]):
  val ops = _ops
  def lift(a: T): W[T]
  def add(a: W[T], b: W[T]): W[T]
  def lower(a: W[T]): T

// If we have access to a given semantics instance, that in turn gives us access
// to a way of performing operations on the wrapped type operated on by semantics,
// since the forwarding is direct.
given [W[_],T, A <: Semantics[W,T]] (using s: A) : Operations[W[T]] with
  def const(a: Double) = s.lift(s.ops.const(a))
  def add(a: W[T], b: W[T]) = s.add(a,b)

// Nothing but a box
case class Direct[T](val value: T)

class DirectSemantics[T](using ops: Operations[T]) extends Semantics[Direct,T]:
  def lift(a: T) : Direct[T] = Direct(a)
  def lower(a: Direct[T]) : T = a.value
  def add(a: Direct[T], b: Direct[T]) = Direct(ops.add(a.value, b.value))

given Operations[Double] with
  def const(a: Double) = a
  def add(a: Double, b: Double) = a + b

// A box, but now with dual numbers
case class JVP[T](val value: T, val tangent: T)

class JVPSemantics[T](using ops: Operations[T]) extends Semantics[JVP, T]:
  def lift(a: T) : JVP[T] = JVP(a,ops.const(0))
  def lower(a: JVP[T]) : T = a.value
  def add(a: JVP[T], b: JVP[T]) =
    val fwd = ops.add(a.value, b.value)
    val jvp = ops.add(a.tangent, b.tangent)
    JVP(fwd, jvp)


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

// Syntactic sugar
extension [T](a: T)(using ops: Operations[T])
  def +(b: T) = ops.add(a, b)


// We have the following function that we want to transform. This
// function is abstract, independent of any context, and basically
// outlines what shold happen to whatever tracer value we plug in.
def foo[T : Operations](x:T) =
  val z = x + x
  z

object Main extends App:
  val f = [T] => (ops: Operations[T]) ?=>
    (x: T) =>
      foo[T](x)

  val df  = deriv(f)
  println(df(3.0))
  val d2f = deriv(deriv(f))
  println(d2f(3.0))
  // And so on:
  // val d3f : Double => Double = deriv(deriv(deriv(g)))
  // val d4f : Double => Double = deriv(deriv(deriv(deriv(g))))
