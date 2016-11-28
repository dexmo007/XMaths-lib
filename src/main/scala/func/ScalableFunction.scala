package func

/**
  * Abstract class for a function that is scaled by a preceding scalar
  */
abstract class ScalableFunction(private var _scalar: BigDecimal = 1) extends Function {

  def scalar: BigDecimal = _scalar

  override def scale(factor: BigDecimal): Unit = {
    _scalar *= factor
  }

  override def scaled(factor: BigDecimal): Function = {
    val scaled = super.clone().asInstanceOf[ScalableFunction]
    scaled.scale(factor)
    scaled
  }
}
