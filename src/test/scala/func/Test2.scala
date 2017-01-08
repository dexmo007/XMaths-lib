package func

import de.hd.func.FunctionParser

/**
  * Created by henri on 12/16/2016.
  */
object Test2 {

  def main(args: Array[String]): Unit = {

    val parse = FunctionParser.parse("pi*sin(x)")

    println(parse.getClass.getSimpleName)
    println(parse)

  }

}
