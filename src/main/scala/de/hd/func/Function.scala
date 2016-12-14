package de.hd.func

import de.hd.func.comb.{FunctionProduct, FunctionSum}
import de.hd.func.exp.{EFunction, ExponentialFunction}
import de.hd.func.log.{LnFunction, LogBaseFunction}
import de.hd.func.trig._

/**
  * trait that defines a mathematical function, a Function is immutable, so all calculation can be done lazily
  */
trait Function extends Cloneable {

  def get(x: BigDecimal): BigDecimal

  def scaled(scalar: BigDecimal): Function

  protected def derive(): Function

  final lazy val derivative: Function = derive()

  def antiderive(c: BigDecimal = 0): Function

  final lazy val antiderivative: Function = antiderive()

  def integral(upper: BigDecimal, lower: BigDecimal): BigDecimal =
    antiderivative.get(upper) - antiderivative.get(lower)

  protected def getConst: Option[BigDecimal]

  final lazy val const: Option[BigDecimal] = getConst

  final lazy val isConst: Boolean = const.isDefined

  lazy val isLinear: Boolean = derivative.isConst

  protected def simplify: Function = if (isConst) Function.const(const.get) else this

  final lazy val simplified: Function = simplify

  def of(inner: Function): Function = ConcatFunction(this, inner)

  def pow(n: Int): Function = Func2Pow(this, n)

  //region standard operations +,-,*,/,unary
  def +(that: Function): Function = FunctionSum(this) + that

  final def -(that: Function): Function = this + -that

  final def unary_- : Function = -1 * this

  def *(that: Function): Function = FunctionProduct(this, that)

  final def *[N: Numeric](factor: N): Function = this.scaled(BigDecimal(factor.toString))

  def /(that: Function): Function = CombinedFunction(this, Operator.DIVIDED_BY, that)

  final def /[N: Numeric](that: N): Function = this.scaled(1 / BigDecimal(that.toString))

  /**
    * handles operation between Function and a numeric
    */
  private[func] trait FunctionScalar {
    def *(that: Function): Function
  }

  final implicit class ScalarBigDecimal(bd: BigDecimal) extends FunctionScalar {
    override def *(that: Function): Function = that.scaled(bd)
  }

  final implicit class ScalarInt(i: Int) extends FunctionScalar {
    override def *(that: Function): Function = that.scaled(i)
  }

  final implicit class ScalarDouble(d: Double) extends FunctionScalar {
    override def *(that: Function): Function = that.scaled(d)
  }

  final implicit class ScalarBigInt(bi: BigInt) extends FunctionScalar {
    override def *(that: Function): Function = that.scaled(BigDecimal(bi))
  }

  final implicit class ScalarFloat(f: Float) extends FunctionScalar {
    override def *(that: Function): Function = that.scaled(BigDecimal(f))
  }

  //endregion

  //region cloneablility
  def cloned(): Function = clone().asInstanceOf[Function]

  override def clone(): AnyRef = super.clone()

  //endregion

  //region equals methods
  def equals(that: Function): Boolean

  override def equals(obj: scala.Any): Boolean = obj match {
    case f: Function =>
      if (getConst.isDefined && f.getConst.isDefined)
        getConst.get == f.getConst.get
      else this.equals(f)
    case _ => false
  }

  //endregion

  //region to string conversion
  def stringify(format: Format): String

  private lazy val texString = stringify(Format.Tex)

  def toTexString: String = texString

  private lazy val plainString = stringify(Format.Plain)

  override def toString: String = plainString

  private lazy val shortString = stringify(Format.ShortPlain)

  def toShortString: String = shortString

  //endregion
}

object Function {

  def cos(scale: BigDecimal = 1): Function = CosineFunction() * scale

  def acos(scale: BigDecimal = 1): Function = ArccosineFunction() * scale

  def sin(scale: BigDecimal = 1): Function = SineFunction() * scale

  def asin(scale: BigDecimal = 1): Function = ArcsineFunction() * scale

  def tan(scale: BigDecimal = 1): Function = TangentFunction() * scale

  def atan(scale: BigDecimal = 1): Function = ArctangentFunction() * scale

  def cot(scale: BigDecimal = 1): Function = scale / TangentFunction()

  def acot(scale: BigDecimal = 1): Function = ArccotangentFunction() * scale

  def ln(scale: BigDecimal = 1): Function = LnFunction() * scale

  def log10(scale: BigDecimal = 1): Function = LogBaseFunction(10) * scale

  def logb(base: Int, scale: BigDecimal = 1): Function = LogBaseFunction(base) * scale

  def linear(b: BigDecimal, a: BigDecimal): Function = Polynomial(b, a)

  def linear(a: BigDecimal = 1): Function = Polynomial(0, a)

  def const(c: BigDecimal): Function = Polynomial(c)

  def toPolynomial(scalars: List[BigDecimal]): Polynomial = Polynomial(scalars)

  def polynomial(first: BigDecimal, scalars: BigDecimal*): Polynomial = Polynomial(first :: scalars.toList)

  def xToN(n: Int, scalar: BigDecimal = 1): Function = toPolynomial(List.fill[BigDecimal](n)(0) ++ List(scalar))

  def from(method: (BigDecimal) => BigDecimal): Function = MethodFunction(method)

  def sqrt(scale: BigDecimal = 1): Function = RootFunction(2) * scale

  def cbrt(scale: BigDecimal = 1): Function = RootFunction(3) * scale

  def nthRoot(n: BigDecimal, scale: BigDecimal = 1): Function = RootFunction(n) * scale

  def exp(function: Function = linear(), scale: BigDecimal = 1): Function = EFunction(function) * scale

  def expb(base: BigDecimal, function: Function = linear(), scale: BigDecimal = 1): Function = ExponentialFunction(base, function) * scale

  implicit def bigDecimalToPolynomial(num: BigDecimal): Function = Polynomial(num)

  implicit def intToPolynomial(num: Int): Function = Polynomial(num)


}