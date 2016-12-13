import de.hd.func.{FunctionParser, _}
import org.scalatest.FunSuite

/**
  * Created by henri on 12/8/2016.
  */
class ParserSpec extends FunSuite {

  test("some parsings") {
    val parse = FunctionParser.parse("sin(x)")
    assert(parse.equals(Function.sin()))
  }

}
