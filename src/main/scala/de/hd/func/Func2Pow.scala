package de.hd.func

/**
  * Created by Henrik on 6/22/2016.
  */
case class Func2Pow[+F <: Function] private[func](inner: F, n: Int, override val scalar: BigDecimal = 1)
  extends ScalarFunction(scalar) {

  override def get(x: BigDecimal): BigDecimal = {
    val innerX = inner.get(x)
    require(!(innerX == 0 && n == 0), "0^0 is undefined!")
    innerX.pow(n) * scalar
  }

  override def scaled(scalar: BigDecimal): Func2Pow[F] = Func2Pow(inner, n, this.scalar * scalar)

  /**
    * derives the function using power rule and chain rule
    *
    * @return derivative
    */
  override protected def derive(): Function = {
    inner.derivative * Func2Pow(inner, n - 1).scaled(scalar * n)
  }

  override def getConst: Option[BigDecimal] = {
    if (n == 0)
      Some(scalar)
    else if (inner.isConst)
      Some(scalar * inner.const.get.pow(n))
    else super.getConst
  }

  override def antiderive(c: BigDecimal): Function = {
    if (!isLinear)
      throw new UnsupportedOperationException("can't find anti-derivative if inner function not linear")
    // todo check if inner.constValue.get * res is needed
    Func2Pow(inner, n + 1).scaled(scalar / (n + 1)) / inner.derivative + c
  }

  override def simplify: Function = inner match {
    case p: Polynomial =>
      var res: Polynomial = p
      for (_ <- 1 to n) res *= p
      res
    case root: RootFunction =>
      if (n == root.n)
        inner.cloned()
      else this
    case _ => this
  }

  override def stringify(format: Format): String =
    format.scalar(scalar) + format.base(inner) + format.pow(n)

  override def equals(that: Function): Boolean = that match {
    case f2p: Func2Pow[Function] =>
      scalar == f2p.scalar && inner.equals(f2p.inner) && n == f2p.n
    case root: RootFunction =>
      scalar == root.scalar && inner == Function.linear() && n == 1 / root.n
    case _ => false
  }

}
