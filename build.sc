import mill._, scalalib._
import mill.scalajslib._

object monet extends ScalaJSModule {
  def scalaVersion = "2.13.4"
  def scalaJSVersion = "1.4.0"
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::1.1.0"
  )

  def scalacOptions = Seq("-deprecation")
  def unmanagedClasspath = T {
    if (!os.exists(millSourcePath / "lib")) Agg()
    else Agg.from(os.list(millSourcePath / "lib").map(PathRef(_)))
  }
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
