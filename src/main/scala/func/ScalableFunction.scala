package func

/**
  * Abstract class for a function that is scaled by a preceding scalar, the scalar is initialized with 1, the identity scalar
  */
abstract class ScalableFunction(private var _scalar: BigDecimal = 1) extends Function {

  def scalar: BigDecimal = _scalar

  override def scaleInternal(factor: BigDecimal): Unit = {
    _scalar *= factor
  }

  def setScalar(newScalar: BigDecimal): ScalableFunction = {
    _scalar = newScalar
    this
  }

  override def constValue: Option[BigDecimal] = if (scalar == 0) Some(0) else None
}
