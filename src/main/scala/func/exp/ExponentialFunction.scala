package func.exp

import func.{Function, Polynomial, ScalableFunction}

/**
  * Created by Henrik on 6/27/2016.
  */
case class ExponentialFunction private[func](base: BigDecimal, inner: Function) extends ScalableFunction {

  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.pow(base.toDouble, inner.get(x).toDouble)
  }

  override def derive(): Function = inner.derive() * Math.log(base.toDouble) * this

  override def antiderive(c: BigDecimal): Function = inner match {
    case p: Polynomial =>
      if (!p.isLinear)
        throw new UnsupportedOperationException

      if (p.scales(1) == 0) {
        Polynomial(c, scalar * Math.pow(base.toDouble, p.scales(0).toDouble))
      } else {
        ExponentialFunction(base, inner) * (scalar / (p.scales(1) * Math.log(base.toDouble)))
      }
    case _ => throw new UnsupportedOperationException
  }
}
