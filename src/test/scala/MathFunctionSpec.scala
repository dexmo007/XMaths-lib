import org.scalatest.FlatSpec

/**
  * Created by henri on 12/21/2016.
  */
abstract class MathFunctionSpec extends FlatSpec {

  trait TestObject[T] {

    def actuals: List[T => Any]

    def expected: List[Any]



  }

}
