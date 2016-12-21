package de.hd.func.impl2

/**
  * A function that is scaled internally, not with a preceding scalar
  *
  * @author Henrik Drefs
  */
trait SelfScaled[+T <: SelfScaled[T]] extends MathFunction {
  this: T =>

  override def *(factor: BigDecimal): T
}
