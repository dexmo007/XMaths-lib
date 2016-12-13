package de.hd.func

import java.util.regex.Pattern

import de.hd.func.FuncUtils._
import de.hd.func.Operator.Operator

/**
  * Created by Henrik on 6/22/2016.
  */
case class CombinedFunction private[func](f1: Function, operator: Operator, f2: Function) extends Function {

  override def get(x: BigDecimal): BigDecimal = operator match {
    case Operator.PLUS => f1.get(x) + f2.get(x)
    case Operator.MINUS => f1.get(x) - f2.get(x)
    case Operator.TIMES => f1.get(x) * f2.get(x)
    case Operator.DIVIDED_BY => f1.get(x) / f2.get(x)
  }

  /**
    * derives the function using sum, product and quotient rule
    *
    * @return derivative
    */
  override def derive(): Function = {
    operator match {
      case Operator.PLUS =>
        f1.derivative + f2.derivative
      case Operator.MINUS =>
        f1.derivative - f2.derivative
      case Operator.TIMES =>
        f1.derivative * f2 + (f1 * f2.derivative)
      case Operator.DIVIDED_BY =>
        (f1.derivative * f2 - (f1 * f2.derivative)) / f2.pow(2)
    }
  }

  override def scaled(scalar: BigDecimal): CombinedFunction = operator match {
    case Operator.PLUS | Operator.MINUS =>
      CombinedFunction(f1.scaled(scalar), operator, f2.scaled(scalar))
    case Operator.TIMES | Operator.DIVIDED_BY =>
      CombinedFunction(f1.scaled(scalar), operator, f2)
  }

  /**
    * anti-derives sums of functions, not possible for product/quotient of functions
    *
    * @param c integration constant
    * @return anti-derivative
    */
  override def antiderive(c: BigDecimal): Function = {
    operator match {
      case Operator.PLUS =>
        f1.antiderive(c) + f2.antiderive(c)
      case Operator.MINUS =>
        f1.antiderive(c) - f2.antiderive(c)
      case Operator.TIMES | Operator.DIVIDED_BY =>
        throw new UnsupportedOperationException("cannot anti-derive product or quotient of functions")
    }
  }

  // todo optimization
  override def stringify(format: Format): String = {
    var res = operator match {
      case Operator.PLUS =>
        val second = f2.stringify(format)
        if (second.startsWith("-"))
          f1.stringify(format) + second
        else
          s"${f1.stringify(format)}+$second"
      case Operator.MINUS =>
        f1.stringify(format) + "-(" + f2.stringify(format) + ")"
      case Operator.TIMES | Operator.DIVIDED_BY =>
        "(" + f1.stringify(format) + ")" + Operator.string(operator) + "(" + f2 + ")"
    }
    val pm = Pattern.quote("+-")
    val mm = Pattern.quote("--")
    val pp = Pattern.quote("++")
    res = res.replaceAll(pm, "-").replaceAll(mm, "+").replaceAll(pp, "+")
    FuncUtils.removeRedundantBraces(res).trimOperators()
  }

  override def getConst: Option[BigDecimal] = operator match {
    case Operator.PLUS =>
      if (f1.isConst && f2.isConst)
        Some(f1.const.get + f2.const.get)
      else if (f1 == f2 * -1) Some(0)
      else None
    case Operator.MINUS =>
      if (f1.isConst && f2.isConst)
        Some(f1.const.get - f2.const.get)
      else if (f1 == f2) Some(0)
      else None
    case Operator.TIMES =>
      if ((f1.isConst && f1.const.get == 0) || (f2.isConst && f2.const.get == 0))
        Some(0)
      else if (f1.isConst && f2.isConst) Some(f1.const.get * f1.const.get)
      else if (f1 == 1 / f2) Some(1)
      else None
    case Operator.DIVIDED_BY =>
      if (f1.isConst && f2.isConst)
        Some(f1.const.get / f2.const.get)
      else if (f1.isConst && f1.const.get == 0) Some(0)
      else if (f1 == f2) Some(1)
      else None
  }

  override def equals(that: Function): Boolean = ???
}

private[func] object Operator extends Enumeration {
  type Operator = Value
  val PLUS, MINUS, TIMES, DIVIDED_BY = Value

  def string(operator: Operator): String = {
    operator match {
      case PLUS => "+"
      case MINUS => "-"
      case TIMES => "*"
      case DIVIDED_BY => "/"
    }
  }
}
