import org.scalatest.{FlatSpec, FunSuite}
import func.Function

/**
  * Tests the equals method for all function implementations
  */
class EqualsSpec extends FunSuite {

  test("exp function equalities") {
    assert(Function.exp() == Function.exp())
    assert(Function.expb(Math.E) == Function.exp())
  }

}
