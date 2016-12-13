package de.hd.func.log

import de.hd.func.{Format, Function}

/**
  * Created by Henrik on 6/25/2016.
  */
case class LnFunction private[func](override val scalar: BigDecimal = 1) extends LogBaseFunction(Math.E, scalar) {


  override def get(x: BigDecimal): BigDecimal = {
    require(x > 0, "ln(" + x + ") is undefined!")
    scalar * Math.log(x.toDouble)
  }

  override def derive(): Function = Function.const(scalar) / Function.linear()

  override def antiderive(c: BigDecimal): Function = Function.linear(scalar) * (LnFunction() - 1) + c

  override def stringify(format: Format): String = format.scalar(scalar) + "ln(x)"
}
