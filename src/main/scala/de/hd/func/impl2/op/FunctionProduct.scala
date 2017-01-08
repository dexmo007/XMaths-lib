package de.hd.func.impl2.op

import de.hd.func.Format
import de.hd.func.FuncUtils._
import de.hd.func.impl2.{MathFunction, SelfScaled}

/**
  * Created by henri on 1/5/2017.
  */
case class FunctionProduct private[op](terms: List[MathFunction]) extends SelfScaled[FunctionProduct] {
  override def apply(x: BigDecimal): BigDecimal = terms.map(f => f(x)).product

  override def *(factor: BigDecimal): FunctionProduct = FunctionProduct(terms.head * factor :: terms.tail)

  override def *(that: MathFunction): MathFunction = that match {
    case product: FunctionProduct => (this *? product).get
    case _ => FunctionProduct(tryMerge(that, terms))

  }

  override def *?(that: FunctionProduct): Option[MathFunction] = {
    def recAdd(res: MathFunction, terms: List[MathFunction]): MathFunction = terms match {
      case Nil => res
      case x :: xs => recAdd(res * x, xs)
    }

    Some(recAdd(this, that.terms))
  }

  private def tryMerge(f: MathFunction, terms: List[MathFunction]): List[MathFunction] = terms match {
    case Nil => List(f)
    case x :: xs => f * x match {
      case FunctionProduct(_) => x :: tryMerge(f, xs)
      case m: MathFunction => m :: xs
    }
  }

  override protected def derive(): MathFunction =
    terms.head.derivative * FunctionProduct(terms.tail) + terms.head * FunctionProduct(terms.tail).derivative

  override def antiDerive(c: BigDecimal): MathFunction = simplified match {
    case FunctionProduct(_) => throw new UnsupportedOperationException
    case f: MathFunction => f.antiDerive(c)
  }

  override protected def getConst: Option[BigDecimal] =
    if (terms.exists(_.isConst(0))) Some(0)
    else if (terms.forall(_.isConst)) Some(terms.map(_.const.get).product)
    else None

  override protected def simplify: MathFunction = {
    val filtered = terms.filterNot(_.const.contains(1))
    if (filtered.size == 1) filtered.head
    else {
      val (constants, rest) = filtered.partition(_.isConst)
      val const = constants.map(_.const.get).product
      val terms =
        if (const == 1) rest
        else rest.head * const :: rest.tail
      if (terms.size == 1)
        terms.head
      else FunctionProduct(terms)
    }
  }

  override def equalsFunction(that: MathFunction): Boolean = ???

  override def stringify(format: Format): String = terms.map(_.stringify(format).maybeBraces) mkString "*"
}

object FunctionProduct {
  def apply(first: MathFunction, second: MathFunction) = new FunctionProduct(List(first, second))
}
