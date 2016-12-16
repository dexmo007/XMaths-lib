package de.hd.func.impl

import de.hd.func.{Format, Function, GenScalarFunction}

import scala.math.BigDecimal

/**
  * Function that represents the nth Root => f(x)=s*x&#94;(1/n)
  */
case class RootFunction private[func](n: BigDecimal, override val scalar: BigDecimal = 1)
  extends GenScalarFunction[RootFunction](scalar) {

  require(n != 0, "0th root does not exist!")

  override def get(x: BigDecimal): BigDecimal = {
    if (n == 2)
      scalar * Math.sqrt(x.toDouble)
    else if (n == 3)
      scalar * Math.cbrt(x.toDouble)
    else
      scalar * Math.pow(x.toDouble, (1 / n).toDouble)
  }

  override def derive(): Function = {
    scalar / RootFunction(n / (1 - n), n)
  }

  override def antiderive(c: BigDecimal): Function = {
    RootFunction(n / (n + 1), scalar * n / (n + 1)) + c
  }

  override def withScalar(newScalar: BigDecimal): RootFunction = copy(scalar = newScalar)

  override def stringify(format: Format): String = format.scalar(scalar) + format.root(n)

  override def equals(that: Function): Boolean = that match {
    case root: RootFunction =>
      n == root.n && scalar == root.scalar
    case _ => false
  }

}
