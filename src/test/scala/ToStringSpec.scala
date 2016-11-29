import org.scalatest.FlatSpec
import func.Function

/**
  * Created by henri on 11/29/2016.
  */
class ToStringSpec extends FlatSpec {

  "Exp func" should "be stringified as" in {
    assert(Function.exp().toString == "e^x")
    assert(Function.exp(Function.linear(3, 2)).toString == "e^(2*x+3)")
    assert(Function.exp(Function.const(1)).toString == "e")
  }

}
