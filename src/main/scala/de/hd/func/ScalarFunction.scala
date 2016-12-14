package de.hd.func

/**
  * Abstract class for a function that is scaled by a preceding scalar, the scalar is initialized with 1, the identity scalar
  */
abstract class ScalarFunction(val scalar: BigDecimal = 1) extends Function {

  override def getConst: Option[BigDecimal] = if (scalar == 0) Some(0) else None

  // todo make abstract to be exact, use copy in implementations
  def withScalar(newScalar: BigDecimal): Function = scaled(1 / scalar)

  def withAddedScalar(that: BigDecimal): Function = scaled((scalar + that) / scalar)

}
