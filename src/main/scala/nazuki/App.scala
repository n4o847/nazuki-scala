package nazuki

import slinky.core._
import slinky.core.facade.Fragment
import slinky.core.facade.Hooks._
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

object App {
  type Props = Unit

  def apply() = component(())

  val component = FunctionalComponent[Props] { props =>
    Fragment(
      h1(s"Hello")
    )
  }
}
