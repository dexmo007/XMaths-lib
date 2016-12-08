package func

import java.util.regex.Pattern
import func.FuncUtils._

import func.Operator.Operator

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
        f1.derive() + f2.derive()
      case Operator.MINUS =>
        f1.derive() - f2.derive()
      case Operator.TIMES =>
        f1.derive() * f2 + (f1 * f2.derive())
      case Operator.DIVIDED_BY =>
        (f1.derive() * f2 - (f1 * f2.derive())) / f2.pow(2)
    }
  }

  override def scale(factor: BigDecimal) {
    operator match {
      case Operator.PLUS | Operator.MINUS =>
        f1.scale(factor)
        f2.scale(factor)
      case Operator.TIMES | Operator.DIVIDED_BY =>
        f1.scale(factor)
    }
  }

  override def scaled(factor: BigDecimal): CombinedFunction = operator match {
    case Operator.PLUS | Operator.MINUS =>
      CombinedFunction(f1.scaled(factor), operator, f2.scaled(factor))
    case Operator.TIMES | Operator.DIVIDED_BY =>
      // todo scale the one that already has a scale of != 1 or 0
      CombinedFunction(f1.scaled(factor), operator, f2)
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
        throw new UnsupportedOperationException("cannot anti-derivce product or quotient of functions")
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
        f1 + "-(" + f2 + ")"
      case Operator.TIMES | Operator.DIVIDED_BY =>
        "(" + f1 + ")" + Operator.string(operator) + "(" + f2 + ")"
    }
    val pm = Pattern.quote("+-")
    val mm = Pattern.quote("--")
    val pp = Pattern.quote("++")
    res = res.replaceAll(pm, "-").replaceAll(mm, "+").replaceAll(pp, "+")
    FuncUtils.removeRedundantBraces(res).trimOperators()
  }

  override def equals(obj: scala.Any): Boolean = {
    if (!obj.isInstanceOf[CombinedFunction]) return false
    val other = obj.asInstanceOf[CombinedFunction]
    f1.equals(other.f1) && operator.equals(other.operator) && f2.equals(other.f2)
  }

  // todo does == call equals??
  override def constValue: Option[BigDecimal] = operator match {
    case Operator.PLUS =>
      if (f1.isConst && f2.isConst)
        Some(f1.constValue.get + f2.constValue.get)
      else if (f1 == f2 * -1) Some(0)
      else None
    case Operator.MINUS =>
      if (f1.isConst && f2.isConst)
        Some(f1.constValue.get - f2.constValue.get)
      else if (f1 == f2) Some(0)
      else None
    case Operator.TIMES =>
      val f1Const = f1.constValue
      val f2Const = f2.constValue
      if ((f1Const.isDefined && f1Const.get == 0) || (f2Const.isDefined && f2Const.get == 0))
        Some(0)
      else if (f1Const.isDefined && f2Const.isDefined) Some(f1Const.get * f2Const.get)
      else if (f1 == 1 / f2) Some(1)
      else None
    case Operator.DIVIDED_BY =>
      val f1Const = f1.constValue
      val f2Const = f2.constValue
      if (f1Const.isDefined && f2Const.isDefined)
        Some(f1Const.get / f2Const.get)
      else if (f1Const.isDefined && f1Const.get == 0) Some(0)
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
