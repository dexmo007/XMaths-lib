import func.{Function, FunctionsSum}

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    val p1 = Function.polynomial(1, 2, 1, 3)
    val p2 = p1.cloned() * 0.5
    println(p2.toShortString)
  }

}
