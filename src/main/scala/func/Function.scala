package func

import func.exp.{AnyExponentialFunction, EFunction}
import func.log.{LnFunction, LogBaseFunction}
import func.trig._

/**
  * Created by Henrik on 6/20/2016.
  */
trait Function extends GenCloneable[Function] {

  def get(x: BigDecimal): BigDecimal

  protected def scaleInternal(factor: BigDecimal): Unit

  final def scale[T: Numeric](factor: T): Unit = scaleInternal(BigDecimal(factor.toString))

  final def scaled[T: Numeric](factor: T): Function = {
    val scaled = cloned()
    scaled.scale(factor)
    scaled
  }

  def derive(): Function

  def antiderive(c: BigDecimal = 0): Function

  def integral(upper: BigDecimal, lower: BigDecimal): BigDecimal = {
    val antideriv = antiderive()
    antideriv.get(upper) - antideriv.get(lower)
  }

  def isConst: Boolean = constValue.isDefined

  def constValue: Option[BigDecimal]

  def isLinear: Boolean = derive().isConst

  def simplified: Function = this

  def of(inner: Function): Function = ConcatFunction(this, inner)

  def pow(n: Int): Function = Func2Pow(this, n)

  //region standard operations +,-,*,/,unary
  def +(that: Function): Function = FunctionsSum(this) + that

  final def -(that: Function): Function = this + -that

  def unary_- : Function = -1 * this

  def *(that: Function): Function = CombinedFunction(this, Operator.TIMES, that)

  final def *[T: Numeric](factor: T): Function = this.scaled(BigDecimal(factor.toString))

  def /(that: Function): Function = CombinedFunction(this, Operator.DIVIDED_BY, that)

  final def /[T: Numeric](div: T): Function = this.scaled(1 / BigDecimal(div.toString))

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
    override def *(that: Function): Function = that.scaled(bi)
  }

  final implicit class ScalarFloat(f: Float) extends FunctionScalar {
    override def *(that: Function): Function = that.scaled(f)
  }

  //endregion

  //region equals methods
  def equals(that: Function): Boolean

  override def equals(obj: scala.Any): Boolean = obj match {
    case f: Function =>
      if (constValue.isDefined && f.constValue.isDefined)
        constValue.get == f.constValue.get
      else this.equals(f)
    case _ => false
  }

  //endregion

  //region to string conversion
  def stringify(format: Format): String

  def toTexString: String = stringify(Format.Tex)

  override def toString: String = stringify(Format.Plain)

  def toShortString: String = stringify(Format.ShortPlain)

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

  def toPolynomial(scalars: Array[BigDecimal]): Polynomial = Polynomial(scalars: _*)

  def toPolynomial(scalars: Array[Double]): Polynomial = Polynomial(scalars.map(d => BigDecimal(d)): _*)

  def toPolynomial(scalars: Array[Int]): Polynomial = Polynomial(scalars.map(i => BigDecimal(i)): _*)

  def polynomial(scalars: BigDecimal*): Polynomial = Polynomial(scalars: _*)

  def xToN(n: Int, scalar: BigDecimal = 1): Function = {
    val scalars = Array.fill[BigDecimal](n + 1)(0)
    scalars(n) = scalar
    toPolynomial(scalars)
  }

  def from(method: (BigDecimal) => BigDecimal): Function = MethodFunction(method)

  def sqrt(scale: BigDecimal = 1): Function = RootFunction(2) * scale

  def cbrt(scale: BigDecimal = 1): Function = RootFunction(3) * scale

  def nthRoot(n: BigDecimal, scale: BigDecimal = 1): Function = RootFunction(n) * scale

  def exp(function: Function = linear(), scale: BigDecimal = 1): Function = EFunction(function) * scale

  def expb(base: BigDecimal, function: Function = linear(), scale: BigDecimal = 1): Function = AnyExponentialFunction(base, function) * scale

  implicit def bigDecimalToPolynomial(num: BigDecimal): Function = Polynomial(num)

  implicit def intToPolynomial(num: Int): Function = Polynomial(num)


}
