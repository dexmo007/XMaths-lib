import org.scalatest.{FlatSpec, FunSuite}
import de.hd.func.Function

/**
  * Tests the equals method for all function implementations
  */
class EqualsSpec extends FunSuite {

  test("exp function equalities") {
    assert(Function.exp() == Function.exp())
    assert(Function.expb(Math.E) == Function.exp())
    // 0 * e^x = 0
    assert(Function.exp() * 0 == Function.const(0))
    // s*1^x = 1
    assert(Function.expb(1) == Function.const(1))
    // s*0^x = 0
    assert(Function.expb(0) == Function.const(0))
  }

}
