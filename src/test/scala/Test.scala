import func.{Format, Func2Pow, Function, Polynomial}

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    val p = Function.sin()
    val p1 = Function.sin(3)
    println(p + p1)
    println(p - p1)
  }

}
