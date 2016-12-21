package func

import de.hd.func.impl2.{MathFunction, Polynomial}

/**
  * Created by henri on 12/17/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
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
}
