package de.hd.func.exp

import de.hd.func._

/**
  * Created by henri on 11/29/2016.
  */
class ExponentialFunction(val base: BigDecimal, val inner: Function, override val scalar: BigDecimal = 1)
  extends ScalarFunction(scalar) {

  if (base == 0 && inner.isConst && inner.const.get == 0)
    throw new ArithmeticException("0^0 is undefined")


  override def get(x: BigDecimal): BigDecimal = {
    if (base == 0 && inner.get(x) == 0)
      throw new ArithmeticException("0^0 is undefined")
    scalar * Math.pow(base.toDouble, inner.get(x).toDouble)
  }

  override def scaled(scalar: BigDecimal): ExponentialFunction = ExponentialFunction(base, inner, this.scalar * scalar)

  override def derive(): Function = inner.derivative * Math.log(base.toDouble) * this

  override def antiderive(c: BigDecimal): Function = inner match {
      // todo generalize isLinear call
    case p: Polynomial =>
      if (!p.isLinear)
        throw new UnsupportedOperationException

      if (p.scalars(1) == 0) {
        Polynomial(c, scalar * Math.pow(base.toDouble, p.scalars.head.toDouble))
      } else {
        ExponentialFunction(base, inner) * (scalar / (p.scalars(1) * Math.log(base.toDouble)))
      }
    case _ => throw new UnsupportedOperationException
  }

  override def getConst: Option[BigDecimal] = {
    if (inner.isConst)
      Some(scalar * Math.pow(base.toDouble, inner.const.get.toDouble))
    else if (base == 0 || base == 1)
      Some(scalar * base)
    else super.getConst
  }

  override def simplify: Function = {
    if (base == 0 || base == 1)
      Function.const(base)
    else if (inner.isConst)
      Function.const(Math.pow(base.toDouble, inner.const.get.toDouble))
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

object ExponentialFunction {
  def apply(base: BigDecimal, inner: Function, scalar: BigDecimal = 1) = new ExponentialFunction(base, inner, scalar)
}
