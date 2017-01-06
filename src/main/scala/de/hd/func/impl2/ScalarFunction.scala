package de.hd.func.impl2

import de.hd.func.Format
import de.hd.func.FuncUtils._
import de.hd.func.impl2.pow.AnyFunc2Pow

/**
  * Created by henri on 12/19/2016.
  */
case class ScalarFunction[T <: ScaledByScalar[T]](scalar: BigDecimal, f: T)
  extends SelfScaled[ScalarFunction[T]] {

  override def apply(x: BigDecimal): BigDecimal = scalar * f(x)

  override def *(factor: BigDecimal): ScalarFunction[T] = copy(scalar = this.scalar * factor)


  override def *?(that: ScalarFunction[T]): Option[MathFunction] =
    if (this.f == that.f) Some(f.pow(2) * (this.scalar * that.scalar))
    else that.f match {
      case AnyFunc2Pow(that.f, n) => Some(f.pow(n + 1) * (this.scalar * that.scalar))
      case _ => None
    }

  override def +?(that: ScalarFunction[T]): Option[ScalarFunction[T]] =
    if (this.f == that.f) Some(copy(scalar = this.scalar + that.scalar))
    else None

  override def +(that: MathFunction): MathFunction =
    if (this.f == that) copy(scalar = scalar + 1)
    else super.+(that)

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

  override def stringify(format: Format): String = s"${format.scalar(scalar)}${f.stringify(format).maybeBraces}"

}