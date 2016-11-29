package func

import func.exp.{EFunction, ExponentialFunction}
import func.log.{LnFunction, LogBaseFunction}
import func.trig._

/**
  * Created by Henrik on 6/20/2016.
  */
trait Function extends Cloneable {

  def get(x: BigDecimal): BigDecimal

  def scale(factor: BigDecimal)

  def scaled(factor: BigDecimal): Function

  def derive(): Function

  def antiderive(c: BigDecimal = 0): Function

  def integral(upper: BigDecimal, lower: BigDecimal): BigDecimal = {
    val antideriv = antiderive()
    antideriv.get(upper) - antideriv.get(lower)
  }

  def isConst: Boolean = constValue.isDefined

  def constValue: Option[BigDecimal]

  def isLinear: Boolean = derive().isConst

  def toTexString: String = {
    toString
  }

  // todo override operators in subclasses to perform fitting addition procedure
  def +(that: Function): Function = CombinedFunction(this, Operator.PLUS, that)

  def -(that: Function): Function = CombinedFunction(this, Operator.MINUS, that)

  def *(that: Function): Function = CombinedFunction(this, Operator.TIMES, that)

  def *(factor: BigDecimal): Function = this.scaled(factor)

  def /(that: Function): Function = CombinedFunction(this, Operator.DIVIDED_BY, that)

  def /(div: BigDecimal): Function = this.scaled(1 / div)

  def pow(n: Int): Function = Func2Pow(this, n)

  def of(inner: Function): Function = ConcatFunction(this, inner)

  implicit class ScalarBigDecimal(bd: BigDecimal) {
    def *(that: Function): Function = that.scaled(bd)
  }

}

object Function {

  def cos(scale: BigDecimal): Function = CosineFunction() * scale

  def cos(): Function = CosineFunction()

  def acos(scale: BigDecimal): Function = ArccosineFunction() * scale

  def acos(): Function = ArccosineFunction()

  def sin(scale: BigDecimal): Function = SineFunction() * scale

  def sin(): Function = SineFunction()

  def asin(scale: BigDecimal): Function = ArcsineFunction() * scale

  def asin(): Function = ArcsineFunction()

  def tan(scale: BigDecimal): Function = TangentFunction() * scale

  def tan(): Function = TangentFunction()

  def atan(scale: BigDecimal): Function = ArctangentFunction() * scale

  def atan(): Function = ArctangentFunction()

  def cot(scale: BigDecimal): Function = scale / TangentFunction()

  def cot(): Function = 1 / TangentFunction()

  def acot(scale: BigDecimal): Function = ArccotangentFunction() * scale

  def acot(): Function = ArccotangentFunction()

  def ln(scale: BigDecimal): Function = LnFunction() * scale

  def ln(): Function = LnFunction()

  def log10(scale: BigDecimal): Function = LogBaseFunction(10) * scale

  def log10(): Function = LogBaseFunction(10)

  def logb(base: Int, scale: BigDecimal): Function = LogBaseFunction(base) * scale

  def logb(base: Int): Function = LogBaseFunction(base)

  def linear(b: BigDecimal, a: BigDecimal): Function = Polynomial(b, a)

  def linear(a: BigDecimal): Function = Polynomial(0, a)

  def linear(): Function = Polynomial(0, 1)

  def const(c: BigDecimal): Function = Polynomial(c)

  def toPolynomial(scalars: Array[BigDecimal]): Function = Polynomial(scalars: _*)

  def toPolynomial(scalars: Array[Double]): Function = Polynomial(scalars.map(d => BigDecimal(d)): _*)

  def toPolynomial(scalars: Array[Int]): Function = Polynomial(scalars.map(i => BigDecimal(i)): _*)

  def polynomial(scalars: BigDecimal*): Function = Polynomial(scalars: _*)

  def xToN(scalar: BigDecimal, n: Int): Function = {
    val scales = Array.fill[BigDecimal](n + 1)(0)
    scales(n) = scalar
    toPolynomial(scales)
  }

  def from(method: (BigDecimal) => BigDecimal): Function = MethodFunction(method)

  def sqrt(scale: BigDecimal): Function = RootFunction(2) * scale

  def sqrt(): Function = RootFunction(2)

  def cbrt(scale: BigDecimal): Function = RootFunction(3) * scale

  def cbrt(): Function = RootFunction(3)

  def nthRoot(n: BigDecimal, scale: BigDecimal): Function = RootFunction(n) * scale

  def nthRoot(n: BigDecimal): Function = RootFunction(n)

  def exp(scale: BigDecimal, function: Function): Function = EFunction(function) * scale

  def exp(function: Function): Function = EFunction(function)

  def exp(scale: BigDecimal): Function = EFunction(linear()) * scale

  def exp(): Function = EFunction(linear())

  def expb(scale: BigDecimal, base: BigDecimal, function: Function): Function = ExponentialFunction(base, function) * scale

  def expb(scale: BigDecimal, base: BigDecimal): Function = ExponentialFunction(base, linear()) * scale

  def expb(base: BigDecimal, function: Function): Function = ExponentialFunction(base, function)

  def expb(base: BigDecimal): Function = ExponentialFunction(base, linear())

  implicit def bigDecimalToPolynomial(num: BigDecimal): Function = Polynomial(num)

  implicit def intToPolynomial(num: Int): Function = Polynomial(num)
}
