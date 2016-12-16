package func

/**
  * Created by henri on 12/16/2016.
  */
case class X(scalar: BigDecimal = 1) extends GenFunction[X] {
  override protected def scaledInternal(scalar: BigDecimal): X = copy(scalar = this.scalar * scalar)

  override def get(x: BigDecimal): BigDecimal = scalar * x
}
