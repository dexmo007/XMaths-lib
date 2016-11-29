import org.scalatest.FlatSpec
import func.Function

/**
  * Tests the equals method for all function implementations
  */
class EqualsSpec extends FlatSpec {

  "Exponential functions" should "equal certain correspondents" in {
    assert(Function.exp() == Function.exp())
    assert(Function.expb(Math.E) == Function.exp())
  }

}
