package func

import de.hd.func.impl2.pow.Func2Pow
import de.hd.func.impl2.{MathFunction, ScalarFunction}

/**
  * Created by henri on 12/17/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    val pow: Func2Pow = Func2Pow(MathFunction.linear(), 2)
    val scaled: ScalarFunction[Func2Pow] = pow * 2
    println(pow.simplified)
  }
}

