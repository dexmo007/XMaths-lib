package func

import func.FuncUtils.MathString

/**
  * Abstract class for a function that is scaled by a preceding scalar, the scalar is initialized with 1, the identity scalar
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

  override def constValue: Option[BigDecimal] = if (scalar == 0) Some(0) else None

  def scalarString: String = scalar.toScalarString
}
