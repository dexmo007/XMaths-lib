package de.hd.func

/**
  * Abstract class for a function that is scaled by a preceding scalar, the scalar is initialized with 1, the identity scalar
  */
trait ScalarFunction extends Function {

  def scalar: BigDecimal

  def withScalar(newScalar: BigDecimal): ScalarFunction

  def withAddedScalar(toAdd: BigDecimal): ScalarFunction

}

abstract class GenScalarFunction[+T <: ScalarFunction](val scalar: BigDecimal) extends GenFunction[T] with ScalarFunction {
  this: T =>

  override def getConst: Option[BigDecimal] = if (scalar == 0) Some(0) else None

  override def withScalar(newScalar: BigDecimal): T

  final override def withAddedScalar(that: BigDecimal): T = withScalar(this.scalar + that)

  final override protected def scaledInternal(factor: BigDecimal): T = withScalar(this.scalar * factor)
}
