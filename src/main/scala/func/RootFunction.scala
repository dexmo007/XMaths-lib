package func

import scala.math.BigDecimal
import func.FuncUtils._

/**
  * Function that represents the nth Root => f(x)=s*x&#94;(1/n)
  */
case class RootFunction private[func](n: BigDecimal) extends ScalableFunction {

  require(n != 0, "0th root does not exist!")

  override def get(x: BigDecimal): BigDecimal = {
    if (n == 2)
      scalar * Math.sqrt(x.toDouble)
    else if (n == 3)
      scalar * Math.cbrt(x.toDouble)
    else
      scalar * Math.pow(x.toDouble, (1 / n).toDouble)
  }

  override def derive(): Function = {
    scalar / RootFunction(n / (1 - n)).scaled(n)
  }

  override def antiderive(c: BigDecimal): Function = {
    RootFunction(n / (n + 1)).scaled(scalar * n / (n + 1)) + c
  }

  override def stringify(format: Format): String = format.scalar(scalar) + format.root(n)
}
