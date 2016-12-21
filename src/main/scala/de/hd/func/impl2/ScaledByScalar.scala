package de.hd.func.impl2

/**
  * A function that is scaled by a preceding scalar
  *
  * @author Henrik Drefs
  */
trait ScaledByScalar[+T <: ScaledByScalar[T]] extends MathFunction {
  this: T =>

  override def *(factor: BigDecimal): ScalarFunction[T] = ScalarFunction(factor, this)
}
