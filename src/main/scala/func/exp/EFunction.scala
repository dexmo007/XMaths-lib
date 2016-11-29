package func.exp

import func.{Function, Polynomial, ScalableFunction}

/**
  * Created by Henrik on 6/27/2016.
  *
  */
case class EFunction private[func](function: Function) extends ScalableFunction {
  //todo make EFunction extend LogBade(E), but override derive and maybe antiDerive, maybe constValue?
  override def get(x: BigDecimal): BigDecimal = {
    scalar * Math.pow(Math.E, function.get(x).toDouble)
  }

  override def derive(): Function = {
    function.derive() * this
  }

  /**
    * anti-derives the function using chain rule only if inner function is linear, else exception is thrown
    *
    * @param c integration constant
    * @return anti-derivative
    */
  override def antiderive(c: BigDecimal): Function = {
    if (!function.isInstanceOf[Polynomial]) throw new UnsupportedOperationException
    val func = function.asInstanceOf[Polynomial]
    if (!func.isLinear) throw new UnsupportedOperationException
    EFunction(function) * (func.scalars(1) * scalar) + c
  }

  override def constValue: Option[BigDecimal] = {
    if (function.isConst)
      Some(scalar * Math.pow(Math.E, function.constValue.get.toDouble))
    else super.constValue
  }
}
