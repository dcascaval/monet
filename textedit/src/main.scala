package textedit

import org.scalajs.dom
import org.scalajs.dom.document

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
    val p = document.createElement("p")
    p.textContent = "hello"
    document.body.appendChild(p)
  }
}
