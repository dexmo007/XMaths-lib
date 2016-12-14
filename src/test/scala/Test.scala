import de.hd.func.Function

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    //    val list = List(Function.sin(2).pow(2), Function.sin(3).pow(1), Function.const(0))
    //    val (matched, rest) = Util.powPartition[TrigonometricFunction](list, _ => true)
    //    println(matched)
    //    println(rest)

    //    println(Function.sin(2) * Function.sin(3))
    val pow = Function.sin().pow(2)
    println(pow)
  }

}
