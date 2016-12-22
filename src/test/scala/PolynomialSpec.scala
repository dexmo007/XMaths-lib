import de.hd.func.impl2.{MathFunction, Polynomial}
import org.scalatest.FlatSpec

/**
  * Created by henri on 12/21/2016.
  */
class PolynomialSpec extends FlatSpec {

  // p1(x) = x^4 + 2x + 3
  val p1 = Polynomial(0 -> 3, 1 -> 2, 4 -> 1)

  "derivative" should "be '4x^3+2'" in {
    assert(p1.derivative == Polynomial(3 -> 4, 0 -> 2))
    assert(p1.derivative.toString == "4*x^3+2")
  }

  "anti-derivative" should "be '1/5x^5+x^2+3x'" in {
    assert(p1.antiDerivative == Polynomial(5 -> 0.2, 2 -> 1, 1 -> 3))
    assert(p1.antiDerivative.toString == "1/5*x^5+x^2+3*x")
  }

  // p(x) = x^4+2x+3
  val p = Polynomial(0 -> 3, 1 -> 2, 4 -> 1)
  println(p)
  //    println("d:" + p.derivative)
  //    println("a:" + p.antiDerivative)
  //    println("level:" + p.level)
  //    println("const:" + p.isConst)
  //    println("simple:" + p.simplified)
  //    println("p+p=" + (p + p))
  //    println("3*p=" + (p * 3))
  //    println("p*p=" + (p * p))
  println("p*const(2)=" + (p * MathFunction.const(2)))


}
