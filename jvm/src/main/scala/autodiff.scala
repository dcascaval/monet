package jax

object Main extends App:
  val f = [T] => (ops: Operations[T]) ?=> (x: T) => foo[T](x) // f(x) = 2x

  val df = deriv(f)
  println(df(3.0)) // 2.0 (slope)
  val d2f = deriv(deriv(f))
  println(d2f(3.0)) // 0.0 (second derivative is constant)
  println("\n")

  // And look, it works even for their test function!
  val g = [T] => (ops: Operations[T]) ?=> (x: T) => bar[T](x) // f(x) = -2sin(x) + x

  println(deriv(g)(3.0))
  println(deriv(deriv(g))(3.0))
  println(deriv(deriv(deriv(g)))(3.0))
  println(deriv(deriv(deriv(deriv(g))))(3.0))

  // Use the same example as in the JAX docs
  println("\n")
  println("Checking sin")

  val s = [T] => (ops: Operations[T]) ?=> (x: T) => sin(x)
  println(deriv(s)(3.0))
  println(deriv(deriv(s))(3.0))
  println(deriv(deriv(deriv(s)))(3.0))
  println(deriv(deriv(deriv(deriv(s))))(3.0))

object TestVJP extends App:
  val f = [T] => (ops: Operations[T]) ?=> (x: T) => bar[T](x) // f(x) = -2sin(x) + x
  println(grad(f)(3.0))

  PRINT = false
  val hess1 = grad(grad(f))(3.0)
  val hess2 = jvp(grad(f), 3.0, 1.0).tangent
  println(hess1)
  println(hess2)

object Main_Fail extends App:
  // This demonstrates why jvp needs to take in an `F` which can handle any type as long as it has Operations.
  // Imagine that it didn't, and instead we concretized a given A.
  def jvp_fail[T, A](f: A => A, arg: T, tangent: T)(using Operations[T], Operations[A]): JVP[T] =
    // Set up a new context
    given semantics: JVPSemantics[T] = new JVPSemantics[T]
    // Lift all of the values to JVP
    val inputs = JVP(arg, tangent)

    // f[JVP[T]](inputs) // ERROR! Who's to say A == JVP[T]?
    ???

  // This function says okay, make the caller give us a function that takes and returns JVP values. Presumably this would be `deriv`.
  def jvp_fail_2[T](f: JVP[T] => JVP[T], arg: T, tangent: T)(using Operations[T]): JVP[T] =
    // Set up a new context
    given semantics: JVPSemantics[T] = new JVPSemantics[T]
    // Lift all of the values to JVP
    val inputs = JVP(arg, tangent)
    f(inputs)

  // Alternatively
  def deriv_fail[T](f: T => T)(using ops: Operations[T]): T => T =
    (x: T) =>
      val jvpf: JVP[T] => JVP[T] = ???
      // ERROR: we can't construct such a value using only the information in `F`, because we've
      // already concretized `f` to a specific type (that is, T, not JVP[T]).
      jvp_fail_2(jvpf, x, ops.const(1)).tangent

  // Works, as long as we take in an `F` that already knows that it's going to undergo a JVP transformation.
  def deriv_fail_2[T](f: JVP[T] => JVP[T])(using ops: Operations[T]): T => T =
    (x: T) => jvp_fail_2(f, x, ops.const(1)).tangent

  // val df : Double => Double = deriv_fail_2[Double](foo)
  // ERROR: we can't give an `f` that knows in advance that it's undergoing JVP, because that's the whole point.
  //    In this case we can provide one because we know the type is `Double`; but in the general case we want to
  //    take the JVP of anything with operations and we don't know _what_ that'll be.
  println("")
