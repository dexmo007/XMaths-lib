package func

/**
  * Created by henri on 12/16/2016.
  */
object Test2 {

  def main(args: Array[String]): Unit = {
    val list: List[MathFunction] = List(X(), X2(), X(2), X2(3), Func2Pow(X()))
    val (m, r) = Util.powPartition[X](list)
    println(m)
    println(r)
  }

}
