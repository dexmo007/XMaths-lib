package func

import de.hd.func.FunctionParser
import de.hd.func.impl2.{MathFunction, Polynomial}

/**
  * Created by henri on 12/17/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    println(FunctionParser.parse("2x").derivative)
  }
}

