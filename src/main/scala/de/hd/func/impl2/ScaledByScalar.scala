package de.hd.func.impl2

import scala.reflect.ClassTag

/**
  * A function that is scaled by a preceding scalar;
  *
  * that means if an instance of `this` multiplied by a number,
  * returns a new instance of `ScalarFunction[T]` with the factor as the scalar
  *
  * @author Henrik Drefs
  */
abstract class ScaledByScalar[T <: ScaledByScalar[T] : ClassTag] extends MathFunction {
  this: T =>

  override def *(factor: BigDecimal): ScalarFunction[T] = ScalarFunction(factor, this)

  override def +(that: MathFunction): MathFunction = that match {
    case ScalarFunction(thatScalar, thatF) =>
      if (this == thatF) ScalarFunction(thatScalar + 1, this)
      else super.+(that)
    case _ => if (this == that) this * 2 else super.+(that)
  }

}
