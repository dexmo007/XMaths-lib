package func

import func.BigDecimalLikePredef.BigDecimalLike

/**
  * Created by henri on 12/16/2016.
  */
object Test2 {

  def doMagic[T: BigDecimalLike](t: T): Unit = {
    println(t)
  }

  def main(args: Array[String]): Unit = {
    doMagic(5)
  }

}
