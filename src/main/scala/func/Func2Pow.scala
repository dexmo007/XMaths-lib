package func

/**
  * Created by Henrik on 6/22/2016.
  */
case class Func2Pow private[func](inner: Function, n: Int) extends ScalableFunction with GenCloneable[Func2Pow] {

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

  override def simplified: Function = inner match {
    case p: Polynomial =>
      var res = p
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
    case f2p: Func2Pow =>
      scalar == f2p.scalar && inner.equals(f2p.inner) && n == f2p.n
    case root: RootFunction =>
      scalar == root.scalar && inner == Function.linear() && n == 1 / root.n
    case _ => false
  }
}
