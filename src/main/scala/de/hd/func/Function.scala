package de.hd.func

import de.hd.func.impl._
import de.hd.func.impl.comb.{FunctionProduct, FunctionQuotient, FunctionSum}
import de.hd.func.impl.exp.{EFunction, ExponentialFunction}
import de.hd.func.impl.log.{LnFunction, LogBaseFunction}
import de.hd.func.impl.trig._

import scala.language.implicitConversions

/**
  * trait that defines a mathematical function, a Function is immutable, so all calculation can be done lazily
  */
trait Function {

  def get(x: BigDecimal): BigDecimal

  protected def derive(): Function

  final lazy val derivative: Function = derive()

  def antiderive(c: BigDecimal = 0): Function

  final lazy val antiderivative: Function = antiderive()

  def integral(upper: BigDecimal, lower: BigDecimal): BigDecimal =
    antiderivative.get(upper) - antiderivative.get(lower)

  protected def getConst: Option[BigDecimal]

  final lazy val const: Option[BigDecimal] = getConst

  final lazy val isConst: Boolean = const.isDefined

  final def isConst(value: BigDecimal): Boolean = const.contains(value)

  lazy val isLinear: Boolean = derivative.isConst

  protected def simplify: Function = if (isConst) Function.const(const.get) else this

  final lazy val simplified: Function = simplify

  def of(inner: Function): Function = CompositeFunction(this, inner)

  def pow(n: Int): Function = Func2Pow(this, n)

  def +(that: Function): Function = FunctionSum(this) + that

  final def -(that: Function): Function = this + -that

  def *(that: Function): Function = FunctionProduct(this, that)

  def *(factor: BigDecimal): Function

  def unary_- : Function = this * -1

  def /(that: Function): Function = FunctionQuotient(this, that)

  def /(that: BigDecimal): Function

  def equals(that: Function): Boolean

  override def equals(obj: scala.Any): Boolean = obj match {
    case f: Function =>
      if (getConst.isDefined && f.getConst.isDefined)
        getConst.get == f.getConst.get
      else this.equals(f)
    case _ => false
  }

  def stringify(format: Format): String

  private lazy val texString = stringify(Format.Tex)

  def toTexString: String = texString

  private lazy val plainString = stringify(Format.Plain)

  override def toString: String = plainString

  private lazy val shortString = stringify(Format.ShortPlain)

  def toShortString: String = shortString
}

object Function {

  def cos(scalar: BigDecimal = 1): Function = CosineFunction() * scalar

  def acos(scalar: BigDecimal = 1): Function = ArccosineFunction() * scalar

  def sin(scalar: BigDecimal = 1): Function = SineFunction() * scalar

  def asin(scalar: BigDecimal = 1): Function = ArcsineFunction() * scalar

  def tan(scalar: BigDecimal = 1): Function = TangentFunction() * scalar

  def atan(scalar: BigDecimal = 1): Function = ArctangentFunction() * scalar

  def cot(scalar: BigDecimal = 1): Function = scalar / TangentFunction()

  def acot(scalar: BigDecimal = 1): Function = ArccotangentFunction() * scalar

  def ln(scalar: BigDecimal = 1): Function = LnFunction() * scalar

  def log10(scalar: BigDecimal = 1): Function = LogBaseFunction(10) * scalar

  def logb(base: Int, scalar: BigDecimal = 1): Function = LogBaseFunction(base) * scalar

  def linear(b: BigDecimal, a: BigDecimal): Function = Polynomial(b, a)

  def linear(a: BigDecimal = 1): Function = Polynomial(0, a)

  def const(c: BigDecimal): Function = Polynomial(c)

  def toPolynomial(scalars: List[BigDecimal]): Polynomial = Polynomial(scalars)

  def polynomial(first: BigDecimal, scalars: BigDecimal*): Polynomial = Polynomial(first :: scalars.toList)

  def xToN(n: Int, scalar: BigDecimal = 1): Function = toPolynomial(List.fill[BigDecimal](n)(0) ++ List(scalar))

  def from(method: (BigDecimal) => BigDecimal): Function = MethodFunction(method)

  def sqrt(scalar: BigDecimal = 1): Function = RootFunction(2, scalar)

  def cbrt(scalar: BigDecimal = 1): Function = RootFunction(3, scalar)

  def nthRoot(n: BigDecimal, scalar: BigDecimal = 1): Function = RootFunction(n) * scalar

  def exp(function: Function = linear(), scalar: BigDecimal = 1): Function = EFunction(function) * scalar

  def expb(base: BigDecimal, function: Function = linear(), scalar: BigDecimal = 1): Function = ExponentialFunction(base, function) * scalar

  //  implicit def bigDecimalToPolynomial(num: BigDecimal): Function = Polynomial(num)
  //
  //  implicit def intToPolynomial(num: Int): Function = Polynomial(num)

  /**
    * handles operation between Function and a numeric
    */
  private[func] trait FunctionScalar {
    def *[F <: GenFunction[F]](that: F): F

    def /(that: Function): Func2Pow
  }

  final implicit class ScalarBigDecimal(bd: BigDecimal) extends FunctionScalar {
    override def *[F <: GenFunction[F]](that: F): F = that * bd

    override def /(that: Function): Func2Pow = Func2Pow(that, -1, bd)
  }

  final implicit class ScalarInt(i: Int) extends FunctionScalar {
    override def *[F <: GenFunction[F]](that: F): F = that * i

    override def /(that: Function): Func2Pow = Func2Pow(that, -1, i)
  }

  final implicit class ScalarDouble(d: Double) extends FunctionScalar {
    override def *[F <: GenFunction[F]](that: F): F = that * d

    override def /(that: Function): Func2Pow = Func2Pow(that, -1, d)
  }

  final implicit class ScalarBigInt(bi: BigInt) extends FunctionScalar {
    override def *[F <: GenFunction[F]](that: F): F = that * BigDecimal(bi)

    override def /(that: Function): Func2Pow = Func2Pow(that, -1, BigDecimal(bi))
  }

  final implicit class ScalarFloat(f: Float) extends FunctionScalar {
    override def *[F <: GenFunction[F]](that: F): F = that * BigDecimal(f)

    override def /(that: Function): Func2Pow = Func2Pow(that, -1, BigDecimal(f))
  }

}
