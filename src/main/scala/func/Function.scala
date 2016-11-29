package func

import func.exp.{AnyExponentialFunction, EFunction}
import func.log.{LnFunction, LogBaseFunction}
import func.trig._
import org.apache.commons.math3.fraction.Fraction
import func.FuncUtils._

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

  def simplified: Function = this

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

  /**
    * handles operation between Function and BigDecimal
    */
  implicit class ScalarBigDecimal(bd: BigDecimal) {
    def *(that: Function): Function = that.scaled(bd)
  }


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
