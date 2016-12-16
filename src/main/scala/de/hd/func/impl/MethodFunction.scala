package de.hd.func.impl

import de.hd.func.{Format, Function, GenScalarFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
case class MethodFunction private[func](method: (BigDecimal) => BigDecimal, override val scalar: BigDecimal = 1)
  extends GenScalarFunction[MethodFunction](scalar) {

  override def get(x: BigDecimal): BigDecimal = scalar * method(x)

  override def derive(): Function = throw new UnsupportedOperationException

  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException

  override def withScalar(newScalar: BigDecimal): MethodFunction = copy(scalar = newScalar)

  override def stringify(format: Format): String = format.scalar(scalar) + s"g$hashCode(x)"

  override def equals(that: Function): Boolean = ???
}
