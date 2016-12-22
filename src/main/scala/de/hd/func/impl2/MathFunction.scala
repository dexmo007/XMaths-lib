package de.hd.func.impl2

import de.hd.func.Format

import scala.language.implicitConversions

/**
  * A representation of a mathematical function; each function instance must be immutable
  *
  * @author Henrik Drefs
  */
trait MathFunction extends (BigDecimal => BigDecimal) {

  override def apply(x: BigDecimal): BigDecimal

  protected def derive(): MathFunction

  lazy val derivative: MathFunction = derive()

  def antiDerive(c: BigDecimal = 0): MathFunction

  lazy val antiDerivative: MathFunction = antiDerive()

  final def integral(lower: BigDecimal, upper: BigDecimal): BigDecimal =
    antiDerivative(upper) - antiDerivative(lower)

  final def integral(lower: BigDecimal, upper: BigDecimal, c: BigDecimal): BigDecimal = {
    val ad = antiDerive(c)
    ad(upper) - ad(lower)
  }

  protected def getConst: Option[BigDecimal]

  final lazy val const: Option[BigDecimal] = getConst

  final lazy val isConst: Boolean = const.isDefined

  final def isConst(c: BigDecimal): Boolean = const.contains(c)

  lazy val isLinear: Boolean = derivative.isConst

  lazy val gradient: BigDecimal = derivative.const.get

  lazy val asLinear: Option[Polynomial] =
    if (isLinear) Some(MathFunction.linear(gradient, this (0)))
    else None

  protected def simplify: MathFunction = this

  lazy val simplified: MathFunction =
    if (isConst) MathFunction.const(const.get)
    else simplify

  // todo conversion of FunctionsSum
  def +(that: MathFunction): MathFunction = ???

  final def unary_- : MathFunction = this * -1

  final def -(that: MathFunction): MathFunction = this + -that

  def *(factor: BigDecimal): MathFunction

  //todo convert FunctionProduct
  def *(that: MathFunction): MathFunction = ???

  def /(divisor: BigDecimal): MathFunction = this * (1 / divisor)

  // todo convert quot
  def /(that: MathFunction): MathFunction = ???

  // todo convert Func2Pow
  def pow(n: Int): MathFunction = ???

  def equalsFunction(that: MathFunction): Boolean

  final override def equals(obj: scala.Any): Boolean = obj match {
    case that: MathFunction =>
      if (isConst && that.isConst) this.const.get == that.const.get
      else equalsFunction(that)
    case _ => const.contains(obj)
  }

  def stringify(format: Format): String

  private lazy val texString = stringify(Format.Tex)

  def toTex: String = texString

  private lazy val plainString = stringify(Format.Plain)

  override def toString(): String = plainString

  private lazy val shortString = stringify(Format.ShortPlain)

  def toShortString: String = shortString
}

object MathFunction {

  def const(c: BigDecimal): Polynomial = Polynomial(c)

  def linear(a: BigDecimal = 1, b: BigDecimal = 0): Polynomial = Polynomial(1 -> a, 0 -> b)

  implicit def constAsPolynomial(c: BigDecimal): Polynomial = Polynomial(c)

}
