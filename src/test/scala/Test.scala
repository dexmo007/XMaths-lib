import func.{Polynomial, RootFunction}

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    val f1 = RootFunction(2,3)
    val f2 = f1.scaled(2)
    f1.scale(3)
    println(f1)
    println(f2)
  }

}
