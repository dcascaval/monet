import mill._, scalalib._
import mill.scalajslib._

object textedit extends ScalaJSModule {
  def scalaVersion = "2.13.4"
  def scalaJSVersion = "1.4.0"
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::1.1.0"
  )
}
