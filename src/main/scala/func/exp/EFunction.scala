package func.exp

import func.{Function, GenCloneable}

/**
  * Created by Henrik on 6/27/2016.
  */
case class EFunction private[func](override val inner: Function)
  extends ExponentialFunction(Math.E, inner) with GenCloneable[EFunction] {

  override def derive(): Function = {
    if (isConst)
      0
    else if (isLinear)
      inner.derive().constValue.get
    else
      inner.derive() * this
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
      constValue.get
    else
      EFunction(inner) * inner.derive().constValue.get + c
  }

  override def constValue: Option[BigDecimal] = {
    if (inner.isConst)
      Some(scalar * Math.pow(Math.E, inner.constValue.get.toDouble))
    else super.constValue
  }


}
