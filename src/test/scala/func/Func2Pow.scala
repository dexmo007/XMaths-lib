package func

/**
  * Created by henri on 12/16/2016.
  */
case class Func2Pow(inner: MathFunction, scalar: BigDecimal = 1) extends GenFunction[Func2Pow] {
  override protected def scaledInternal(scalar: BigDecimal): Func2Pow = copy(scalar = this.scalar * scalar)

  override def get(x: BigDecimal): BigDecimal = scalar * inner.get(x)
}
