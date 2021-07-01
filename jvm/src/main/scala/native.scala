package monet

object Main extends App:
  println("Hello World")

  given DiffContext = new DiffContext()
  val a = 2.v
  val b = 2.v
  val exp = (a+b)*(a+b)
  println(exp.d(Seq(a,b),1.0))
