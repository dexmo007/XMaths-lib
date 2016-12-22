package de.hd.func.impl2

/**
  * A function that is scaled internally, not with a preceding scalar;
  *
  * that means an instance of this function multiplied by a number returns a new scaled instance of this type
  *
  * @author Henrik Drefs
  */
trait SelfScaled[+T <: SelfScaled[T]] extends MathFunction {
  this: T =>

  override def *(factor: BigDecimal): T
}
