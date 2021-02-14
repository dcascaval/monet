import mill._, scalalib._
import mill.scalajslib._

object typings extends ScalaJSModule {
  def scalaVersion = "2.13.4"
  def scalaJSVersion = "1.4.0"
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::1.1.0"
  )
}

object monet extends ScalaJSModule {
  def scalaVersion = "2.13.4"
  def scalaJSVersion = "1.4.0"
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::1.1.0"
  )
  def moduleDeps = Seq(typings)
  def scalacOptions = Seq("-deprecation")
}

def WIN = System.getProperty("os.name") contains ("Win")

def minifier = T {
  os.makeDir.all(T.dest)
  os.proc(
    os.pwd / 'node_modules / ".bin" / "esbuild" + (if (WIN) ".cmd" else ""),
    "--minify",
    monet.fastOpt().path,
    s"--outfile=${T.dest}/out.min.js"
  ).call(cwd = os.pwd)
  PathRef(T.dest)
}
