import func.Function

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    println(Function.expb(0, Function.const(1)) == Function.const(0))
  }

}
