package de.hd.func.impl.log

import de.hd.func.{Format, Function, GenScalarFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
class LogBaseFunction private[func](val base: BigDecimal, override val scalar: BigDecimal = 1) extends GenScalarFunction[LogBaseFunction](scalar) {

  require(base > 1, "Base must not be " + base)

  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "log" + base + "(" + x + ") is undefined!")
    scalar * Math.log(x.toDouble) / Math.log(base.toDouble)
  }

  // todo check if correct?
  override def derive(): Function = scalar / Function.linear(Math.log(base.toDouble))

  // todo check if possible
  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException

  override def withScalar(newScalar: BigDecimal): LogBaseFunction = LogBaseFunction(base, newScalar)

  override def equals(that: Function): Boolean = that match {
    case log: LogBaseFunction =>
      base == log.base && scalar == log.scalar
    case _ => false
  }

  override def stringify(format: Format): String = format.scalar(scalar) + s"log$base(x)"
}

object LogBaseFunction {
  def apply(base: BigDecimal, scalar: BigDecimal = 1): LogBaseFunction = new LogBaseFunction(base, scalar)
}
