import func.FunctionParser

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    val parse = FunctionParser.parse("y = pi*x")

    println(parse.toString)
  }

}
