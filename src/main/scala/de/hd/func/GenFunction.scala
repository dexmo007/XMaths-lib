package de.hd.func

/**
  *
  */
trait GenFunction[+T <: Function] extends Function {
  this: T =>

  protected def scaledInternal(factor: BigDecimal): T

  //todo own numeric type class to enable asBigDecimal feature
  final override def *(factor: BigDecimal): T = scaledInternal(BigDecimal(factor.toString))

  final override def /(that: BigDecimal): T = scaledInternal(1 / BigDecimal(that.toString))

  override def unary_- : T = this * -1
}