import func.{Function, FunctionsSum}

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    println(Function.sin() + Function.sqrt() + Function.sin(3) + Function.sqrt(0.5))
  }

}
