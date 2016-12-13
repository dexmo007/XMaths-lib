package de.hd.func.log

import de.hd.func.{Format, Function, ScalarFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
class LogBaseFunction private[func](val base: BigDecimal, override val scalar: BigDecimal = 1) extends ScalarFunction(scalar) {

  require(base > 1, "Base must not be " + base)


  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "log" + base + "(" + x + ") is undefined!")
    scalar * Math.log(x.toDouble) / Math.log(base.toDouble)
  }

  override def scaled(scalar: BigDecimal): LogBaseFunction = LogBaseFunction(base, this.scalar * scalar)

  // todo check if correct?
  override def derive(): Function = scalar / Function.linear(Math.log(base.toDouble))

  // todo check if possible
  override def antiderive(c: BigDecimal): Function = ???

  override def stringify(format: Format): String = format.scalar(scalar) + s"log$base(x)"

  override def equals(that: Function): Boolean = that match {
    case log: LogBaseFunction =>
      base == log.base && scalar == log.scalar
    case _ => false
  }
}

object LogBaseFunction {
  def apply(base: BigDecimal, scalar: BigDecimal = 1): LogBaseFunction = new LogBaseFunction(base, scalar)
}
