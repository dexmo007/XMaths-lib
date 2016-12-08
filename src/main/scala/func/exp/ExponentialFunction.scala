package func.exp

import func.FuncUtils._
import func.{Format, FuncUtils, Function, ScalableFunction}

/**
  * Created by henri on 11/29/2016.
  */
abstract class ExponentialFunction(_base: BigDecimal, _inner: Function) extends ScalableFunction {

  if (base == 0 && inner.isConst && inner.constValue.get == 0)
    throw new ArithmeticException("0^0 is undefined")

  def base: BigDecimal = _base

  def inner: Function = _inner

  override def get(x: BigDecimal): BigDecimal = {
    if (base == 0 && inner.get(x) == 0)
      throw new ArithmeticException("0^0 is undefined")
    scalar * Math.pow(base.toDouble, inner.get(x).toDouble)
  }

  override def simplified: Function = {
    if (base == 0 || base == 1)
      Function.const(base)
    else if (inner.isConst)
      Function.const(Math.pow(base.toDouble, inner.constValue.get.toDouble))
    else this
  }

  override def equals(that: Function): Boolean = that match {
    case that: ExponentialFunction =>
      base == that.base && inner == that.inner && scalar == that.scalar
    case _ => false
  }

  override def stringify(format: Format): String = format.scalar(scalar) +
    s"${format.base(base)}${format.pow(inner)}"

}
