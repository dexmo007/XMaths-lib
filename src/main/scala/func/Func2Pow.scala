package func

import func.FuncUtils._

/**
  * Created by Henrik on 6/22/2016.
  */
case class Func2Pow private[func](inner: Function, n: Int) extends ScalableFunction {

  override def get(x: BigDecimal): BigDecimal = {
    val innerX = inner.get(x)
    require(!(innerX == 0 && n == 0), "0^0 is undefined!")
    innerX.pow(n) * scalar
  }

  /**
    * derives the function using power rule
    *
    * @return derivative
    */
  override def derive(): Function = {
    inner.derive() * Func2Pow(inner, n - 1).scaled(scalar * n)
  }

  override def constValue: Option[BigDecimal] = {
    if (n == 0)
      Some(scalar)
    else if (inner.isConst)
      Some(scalar * inner.constValue.get.pow(n))
    else super.constValue
  }

  override def antiderive(c: BigDecimal): Function = {
    if (!isLinear)
      throw new UnsupportedOperationException("can't find anti-derivative if inner function not linear")
    // todo check if inner.constValue.get * res is needed
    Func2Pow(inner, n + 1).scaled(scalar / (n + 1)) / inner.derive() + c
  }

  override def stringify(format: Format): String =
    format.scalar(scalar) + format.base(inner) + format.pow(n)

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Func2Pow =>
        scalar == that.scalar && inner.equals(that.inner) && n == that.n
      case _ => false
    }
  }

  override def equals(that: Function): Boolean = ???
}
