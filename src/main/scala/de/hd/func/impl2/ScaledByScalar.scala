package de.hd.func.impl2

/**
  * A function that is scaled by a preceding scalar;
  *
  * that means if an instance of `this` multiplied by a number,
  * returns a new instance of `ScalarFunction[T]` with the factor as the scalar
  *
  * @author Henrik Drefs
  */
trait ScaledByScalar[+T <: ScaledByScalar[T]] extends MathFunction {
  this: T =>

  override def *(factor: BigDecimal): ScalarFunction[T] = ScalarFunction(factor, this)
}
