package de.hd.func.impl2.op

import de.hd.func.Format
import de.hd.func.impl2.{MathFunction, SelfScaled}

/**
  * A sum of all kinds of functions.
  *
  * @author Henrik Drefs
  */
case class FunctionSum private[op](addends: List[MathFunction]) extends SelfScaled[FunctionSum] {

  override def apply(x: BigDecimal): BigDecimal = addends.map(_ (x)).sum

  override def +?(that: FunctionSum): Option[FunctionSum] = {
    def recAdd(res: FunctionSum, terms: List[MathFunction]): FunctionSum = terms match {
      case Nil => res
      case x :: xs => recAdd(res + x, xs)
    }

    Some(recAdd(this, that.addends))
  }

  override def +(that: MathFunction): FunctionSum = that match {
    case sum: FunctionSum => (this +? sum).get
    case _ => FunctionSum(tryMerge(that, addends))
  }

  /**
    * @param f     the function to be merged (through adding) with the list `terms`
    * @param terms list be merged into
    * @return a list with `f` either added to or merge with a member
    */
  private def tryMerge(f: MathFunction, terms: List[MathFunction]): List[MathFunction] = terms match {
    case Nil => List(f)
    case x :: xs => f + x match {
      case FunctionSum(_) => x :: tryMerge(f, xs)
      case m: MathFunction => m :: xs
    }
  }

  override def *(factor: BigDecimal): FunctionSum = copy(addends = addends.map(_ * factor))

  override protected def derive(): MathFunction = copy(addends = addends.map(_.derivative))

  override def antiDerive(c: BigDecimal): MathFunction = copy(addends = addends.map(_.antiDerivative)) + c

  override protected def getConst: Option[BigDecimal] =
    if (addends.isEmpty) Some(0)
    else if (addends.forall(_.isConst)) Some(addends.map(_.const.get).sum)
    else None

  override protected def simplify: MathFunction = {
    val filtered = addends.filter(!_.isConst(0))
    if (filtered.size == 1)
      return filtered.head
    val (constants, rest) = filtered.partition(_.isConst)
    val const = constants.map(_.const.get).sum
    if (const != 0)
      FunctionSum(MathFunction.const(const) :: rest)
    else FunctionSum(rest)
  }

  override def equalsFunction(that: MathFunction): Boolean = ???

  override def stringify(format: Format): String = simplified match {
    case FunctionSum(terms) => terms map (_.stringify(format)) mkString "+"
    case _ => simplified.stringify(format)
  }
}

object FunctionSum {
  def apply(first: MathFunction, second: MathFunction): FunctionSum = FunctionSum(List(first, second))

  def apply(): FunctionSum = FunctionSum(List(MathFunction.const(0)))
}
