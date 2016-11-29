package func

/**
  * Created by Henrik on 6/25/2016.
  */
case class MethodFunction private[func](method: (BigDecimal) => BigDecimal) extends ScalableFunction {

  override def get(x: BigDecimal): BigDecimal = scalar * method(x)

  override def derive(): Function = throw new UnsupportedOperationException

  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException
}
