package de.hd.func.impl.exp

import de.hd.func._

/**
  * Created by Henrik on 6/27/2016.
  */
case class EFunction private[func](override val inner: Function, override val scalar: BigDecimal = 1)
  extends ExponentialFunction(Math.E, inner, scalar) {

  override def derive(): Function = {
    if (isConst)
      Function.const(0)
    else if (isLinear)
      Function.const(inner.derivative.const.get)
    else
      inner.derivative * this
  }

  /**
    * @param c integration constant
    * @return anti-derivative
    * @throws UnsupportedOperationException if the inner function is not linear
    */
  override def antiderive(c: BigDecimal): Function = {
    if (!isLinear)
      throw new UnsupportedOperationException
    if (isConst)
      Function.linear(c, const.get)
    else
      EFunction(inner) * inner.derivative.const.get + Function.const(c)
  }

  override def withScalar(newScalar: BigDecimal): EFunction = copy(scalar = newScalar)

  override def getConst: Option[BigDecimal] = {
    if (inner.isConst)
      Some(scalar * Math.pow(Math.E, inner.const.get.toDouble))
    else super.getConst
  }


}
