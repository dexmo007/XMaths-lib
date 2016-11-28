package func

import func.FuncUtils.MathString

/**
  * Created by Henrik on 6/22/2016.
  */
case class Func2Pow private[func](function: Function, n: Int) extends ScalableFunction(1) {

  override def get(x: BigDecimal): BigDecimal = {
    val innerX = function.get(x)
    require(!(innerX == 0 && n == 0), "0^0 is undefined!")
    innerX.pow(n) * scalar
  }

  /**
    * derives the function using power rule
    *
    * @return derivitive
    */
  override def derive(): Function = {
    function.derive() * Func2Pow(function, n - 1).scaled(scalar * n)
  }

  /**
    * antiderives the function using chain rule
    *
    * @param c integration constant
    * @return antiderivitive
    */
  override def antiderive(c: BigDecimal): Function = function match {
      // todo when isLinear was removed to Function
    case p: Polynomial =>
      if (p.isLinear)
        Func2Pow(function, n + 1).scaled(scalar / (n + 1)) / function.derive() + c
      throw new UnsupportedOperationException("Antideriving only possible if inner function is linear!")
    case _ =>
      throw new UnsupportedOperationException("Antideriving only possible if inner function is linear!")
  }

  override def toString: String = {
    scalar.toScalarString + "(" + function + ")^" + n
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Func2Pow =>
        scalar == that.scalar && function.equals(that.function) && n == that.n
      case _ => false
    }
  }
}
