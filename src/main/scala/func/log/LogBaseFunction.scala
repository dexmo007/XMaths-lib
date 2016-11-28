package func.log

import func.{Function, ScalableFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
case class LogBaseFunction private[func](base: Int) extends ScalableFunction {

  require(base > 0 && base != 1, "Base must not be " + base)

  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "log" + base + "(" + x + ") is undefined!")
    scalar * Math.log(x.toDouble) / Math.log(base)
  }

  // todo check if correct?
  override def derive(): Function = {
    scalar / Function.linear(Math.log(base))
  }

  override def antiderive(c: BigDecimal): Function = ???
}
