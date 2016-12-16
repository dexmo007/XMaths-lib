package func

/**
  * Created by henri on 12/16/2016.
  */
case class X2(scalar: BigDecimal = 1) extends GenFunction[X2] {
  override protected def scaledInternal(scalar: BigDecimal): X2 = copy(scalar = this.scalar * scalar)

  override def get(x: BigDecimal): BigDecimal = scalar * x * x
}
