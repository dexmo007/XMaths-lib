import de.hd.func.Function

/**
  * Created by henri on 11/25/2016.
  */
object Test {

  def main(args: Array[String]): Unit = {
    println(Function.linear().pow(2) + (Function.linear().pow(2) * 3))
  }

}
