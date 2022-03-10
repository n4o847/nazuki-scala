package nazuki

import slinky.core._
import slinky.core.facade.Fragment
import slinky.core.facade.Hooks._
import slinky.web.html._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import nazuki.script._

@JSImport("resources/App.css", JSImport.Default)
@js.native
object AppCSS extends js.Object

object App {
  type Props = Unit

  @inline def apply() = component(())

  val component = FunctionalComponent[Props] { props =>
    val (source, setSource) = useState("""
      |1 + 2
      |a = 1
    """.stripMargin.trim() + "\n")
    val (result, setResult) = useState("")

    useEffect(
      () => {
        handleCompile()
      },
      Nil
    )

    def handleCompile() = {
      println(source)

      val tokens = Lexer.tokenize(source)
      println(tokens)

      val tree = Parser.parse(source)
      println(tree)

      val result = s"${tokens}\n\n${tree}"
      setResult(result)
    }

    Fragment(
      h1(s"Hello"),
      textarea(
        style := js.Dynamic.literal(
          boxSizing = "border-box",
          width = "100%",
          height = "10rem"
        ),
        value := source,
        onChange := { e => setSource(e.target.value) }
      ),
      button(
        onClick := { () => handleCompile() }
      )(
        s"Compile"
      ),
      textarea(
        style := js.Dynamic.literal(
          boxSizing = "border-box",
          width = "100%",
          height = "10rem"
        ),
        readOnly,
        defaultValue := result
      )
    )
  }
}
