package func.log

import func.{Format, Function, GenCloneable, ScalableFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
class LogBaseFunction private[func](private val _base: BigDecimal) extends ScalableFunction with GenCloneable[LogBaseFunction] {

  require(_base > 1, "Base must not be " + _base)

  def base: BigDecimal = _base

  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "log" + base + "(" + x + ") is undefined!")
    scalar * Math.log(x.toDouble) / Math.log(base.toDouble)
  }

  // todo check if correct?
  override def derive(): Function = {
    scalar / Function.linear(Math.log(base.toDouble))
  }

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
  def apply(base: Int): LogBaseFunction = new LogBaseFunction(base)
}
