package de.hd.func.impl

import de.hd.func.{Format, Function, GenFunction}

/**
  * Created by Henrik on 6/25/2016.
  */
case class ConcatFunction private[func](outer: Function, inner: Function) extends GenFunction[ConcatFunction] {

  override def get(x: BigDecimal): BigDecimal = outer.get(inner.get(x))

  override protected def derive(): Function = inner.derivative * outer.derivative.of(inner)

  // todo possible? if not throw unsupported
  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException

  override protected def scaledInternal(factor: BigDecimal): ConcatFunction = ConcatFunction(outer * factor, inner)

  override def getConst: Option[BigDecimal] = if (outer.isConst) outer.const else inner.const

  // todo check if consistent to all possibilities
  override def stringify(format: Format): String = {
    if (outer.isInstanceOf[Polynomial])
      outer.toString.replaceAll("x", "(" + inner + ")")
    else
      outer.toString.replaceAll("x", inner.toString)
  }

  override def simplify: Function = {
    if (getConst.isDefined)
      Function.const(getConst.get)
    else if (inner == Function.linear())
      outer
    else
      this
  }

  override def equals(that: Function): Boolean = ???
}
