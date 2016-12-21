package de.hd.func.impl2

import de.hd.func.Format

/**
  * Created by henri on 12/19/2016.
  */
case class ScalarFunction[+T <: ScaledByScalar[T]](scalar: BigDecimal, f: T) extends SelfScaled[ScalarFunction[T]] {

  override def apply(x: BigDecimal): BigDecimal = scalar * f(x)

  override def *(factor: BigDecimal): ScalarFunction[T] = copy(scalar = this.scalar * factor)

  override protected def derive(): MathFunction = f.derivative * scalar

  override def antiDerive(c: BigDecimal): MathFunction = f.antiDerive(c) * scalar

  override protected def getConst: Option[BigDecimal] =
    if (scalar == 0) Some(0)
    else f.const.map(_ * scalar)

  override protected def simplify: MathFunction =
    if (scalar == 1) f.simplified
    else f.simplified * scalar

  override def equalsFunction(that: MathFunction): Boolean = that match {
    case ScalarFunction(thatScalar, thatF) => this.scalar == thatScalar && this.f == thatF
    case _ => scalar == 1 && this.f == that
  }

  // todo check if paren are needed (+- that are not itself in parentheses?)
  override def stringify(format: Format): String = s"${format.scalar(scalar)}(${f.stringify(format)})"
}
