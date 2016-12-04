package func.trig

import func.{Format, Function, ScalableFunction}

/**
  * Superclass for all trigonometric functions
  */
abstract class TrigonometricFunction extends ScalableFunction {

  val name: String

  override def stringify(format: Format): String = format.scalar(scalar) + s"$name(x)"

}
