package func

import func.FuncUtils._

/**
  * Created by Henrik on 6/25/2016.
  */
case class ConcatFunction private[func](outer: Function, inner: Function) extends Function {

  override def get(x: BigDecimal): BigDecimal = {
    outer.get(inner.get(x))
  }

  override def derive(): Function = {
    inner.derive() * outer.derive().of(inner)
  }

  override def scale(factor: BigDecimal): Unit = {
    outer.scale(factor)
  }

  override def scaled(factor: BigDecimal) = ConcatFunction(outer.scaled(factor), inner)

  // todo possible? if not throw unsupported
  override def antiderive(c: BigDecimal): Function = ???

  override def constValue: Option[BigDecimal] = if (outer.isConst) outer.constValue else inner.constValue

  override def toString: String = {
    if (outer.isInstanceOf[Polynomial])
      outer.toString.replaceAll("x", "(" + inner + ")")
    else
      outer.toString.replaceAll("x", inner.toString)
  }
}
