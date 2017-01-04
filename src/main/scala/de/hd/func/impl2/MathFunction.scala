package de.hd.func.impl2

import de.hd.func.Format
import de.hd.func.impl2.log.{LnFunction, Log10Function, LogBaseFunction}
import de.hd.func.impl2.op.{FunctionQuotient, FunctionSum}
import de.hd.func.impl2.pow.{AnyFunc2Pow, CbrtFunction, Func2Pow, SqrtFunction}
import de.hd.func.impl2.trig.{CosineFunction, SineFunction}

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
    if (isConst) const.get
    else simplify

  def +(that: MathFunction): MathFunction = FunctionSum() + this + that

  final def unary_- : MathFunction = this * -1

  final def -(that: MathFunction): MathFunction = this + -that

  def *(factor: BigDecimal): MathFunction

  //todo convert FunctionProduct
  def *(that: MathFunction): MathFunction =
    if (this == that) this pow 2
    else if (that.isConst) this * that.const.get
    else ???

  def /(divisor: BigDecimal): MathFunction = this * (1 / divisor)

  def /(that: MathFunction): MathFunction = that match {
    case FunctionQuotient(dividend, divisor) => (this * divisor) / dividend
    case _ => FunctionQuotient(this, that)
  }

  def pow(n: Int): MathFunction = Func2Pow(this, n)

  def pow(n: BigDecimal): MathFunction = AnyFunc2Pow(this, n)

  def equalsFunction(that: MathFunction): Boolean

  final override def equals(obj: scala.Any): Boolean = obj match {
    case that: MathFunction =>
      if (isConst && that.isConst) this.const.get == that.const.get
      else simplified.equalsFunction(that.simplified)
    case _ => const.contains(obj)
  }

  def stringify(format: Format): String

  private lazy val texString = stringify(Format.Tex)

  def toTex: String = texString

  private lazy val plainString = stringify(Format.Plain)

  override def toString(): String = plainString

  private lazy val shortString = stringify(Format.ShortPlain)

  def toShortString: String = shortString

  final implicit class BigDecImplicit(bd: BigDecimal) {
    def x: Polynomial = Polynomial(1 -> bd)
  }

  final implicit class BigDecPow(bd: BigDecimal) {
    def pow(n: BigDecimal): BigDecimal = math.pow(bd.toDouble, n.toDouble)
  }

}

object MathFunction {

  def const(c: BigDecimal): Polynomial = Polynomial(c)

  def linear(a: BigDecimal = 1, b: BigDecimal = 0): Polynomial = Polynomial(1 -> a, 0 -> b)

  val x: Polynomial = linear()

  def cos(f: MathFunction = x): MathFunction = CosineFunction(f)

  def acos(): MathFunction = ???

  def sin(f: MathFunction = x): MathFunction = SineFunction(f)

  def asin(): MathFunction = ???

  def tan(): MathFunction = ???

  def atan(): MathFunction = ???

  def cot(): MathFunction = ???

  def acot(): MathFunction = ???

  def ln(f: MathFunction = x): MathFunction = LnFunction(f)

  def log10(f: MathFunction = x): MathFunction = Log10Function(f)

  def logb(base: BigDecimal, f: MathFunction = x): MathFunction = LogBaseFunction(base, f)

  def from(method: (BigDecimal) => BigDecimal): MathFunction = ???

  def sqrt(f: MathFunction = x): MathFunction = SqrtFunction(f)

  def cbrt(f: MathFunction = x): MathFunction = CbrtFunction(f)

  def exp(f: MathFunction = x): MathFunction = ???

  def expb(base: BigDecimal, function: MathFunction = x): MathFunction =
    ???

  implicit def constAsPolynomial(c: BigDecimal): Polynomial = Polynomial(c)


}
