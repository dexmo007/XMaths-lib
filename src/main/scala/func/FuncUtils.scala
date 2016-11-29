package func

import func.FunctionParser.FunctionParseException
import func.trig.TrigonometricFunction
import org.apache.commons.math3.fraction.Fraction

import scala.collection.mutable

/**
  * todo move implicits to function object? partly done
  */
object FuncUtils {

  def toFraction(num: BigDecimal): String = new Fraction(num.toDouble, 1e-15, 100).toString

  /**
    * @param s    reference to string
    * @param open index of open-parentheses
    * @return index of the corresponding close parentheses
    */
  def findCloseParen(s: String, open: Int): Int = {
    val stack = new mutable.Stack[Char]
    var i = open
    while (i < s.length) {
      val c = s(i)
      c match {
        case '(' => stack.push(c)
        case ')' => stack.pop()
        case _ =>
      }
      if (stack.isEmpty) {
        return i
      }
      i += 1
    }
    throw FunctionParseException("No closing parentheses!")
  }

  def removeRedundantBraces(s: String): String = {
    var res = s
    var i = 0
    // todo use findCloseParen
    while (res.indexOf("(", i) != -1) {
      if (res.indexOf(")", i) == i + 2 && i > 0 && !res(i - 1).isLetter) {
        res = res.substring(0, i) + res(i + 1) + res.substring(i + 3)
      }
      i += 1
    }
    res
  }

  def powString(f: Function): String = {
    if (f.isConst && f.constValue.get == 1)
      return ""
    var s = f.toString
    if (s.length > 1 && !s.containsNoOps)
      s = s.wrap
    s"^$s"
  }

  def powString(pow: BigDecimal): String = {
    if (pow == 1)
      return ""
    val s = pow.toMathString
    if (s.contains("/") || s.contains("/"))
      s.wrap
    else s
  }

  def baseString(base: BigDecimal): String = {
    val s = base.toMathString
    if (s.startsWith("-") || s.contains("/") || s.contains("*"))
      s.wrap
    else
      s
  }

  def baseString(f: Function): String = {
    if (f.isConst)
      baseString(f.constValue.get)
    else f match {
      case trig: TrigonometricFunction =>
        if (trig.scalar == 1)
          trig.toString
        else
          trig.toString.wrap
      case _ => f.toString.wrap
    }
  }

  /**
    * handles string formatting of numbers in this (mathematical) context
    */
  implicit class MathString(num: BigDecimal) {
    def toMathString: String = {
      if (num == 0) {
        "0"
      } else if (num == Math.PI) {
        "pi"
      } else if (num % Math.PI == 0) {
        (num / Math.PI).toMathString + "*pi"
      } else if (num == Math.E) {
        "e"
      } else if (num % Math.E == 0) {
        (num / Math.E).toMathString + "*e"
      } else if (num.isValidInt) {
        num.toInt.toString
      } else {
        val frac = new Fraction(num.toDouble, 1e-15, 100)
        if (frac.getDenominator > 1000)
          num.toString
        else
          frac.toString.replaceAll(" ", "")
      }
    }

    def toTexString: String = {
      if (num == 0) {
        "0"
      } else if (num == Math.PI) {
        "\\pi"
      } else if (num % Math.PI == 0) {
        (num / Math.PI).toTexString + "\\pi"
      } else if (num == Math.E) {
        "e"
      } else if (num % Math.E == 0) {
        (num / Math.E).toTexString + "e"
      } else if (num.isValidInt) {
        num.toInt.toString
      } else {
        val frac = new Fraction(num.toDouble, 1e-15, 100)
        if (frac.getDenominator > 1000) {
          num.toString
        } else if ((frac.getNumerator.toDouble / frac.getDenominator).isValidInt) {
          (frac.getNumerator.toDouble / frac.getDenominator).toInt.toString
        } else {
          "\\frac{" + frac.getNumerator + "}{" + frac.getDenominator + "}"
        }
      }
    }

    def toScalarString: String = {
      if (num == 1) ""
      else if (num == -1) "-"
      else num.toMathString + "*"
    }
  }

  implicit class FuncString(s: String) {
    val plus: Char = '+'
    val minus: Char = '-'
    val times: Char = '*'
    val div_by: Char = '/'
    val pow: Char = '^'

    def containsNoOps: Boolean = s.forall(c => c != plus && c != minus && c != times && c != div_by && c != pow)

    def wrap: String = s"($s)"

    def unwrap: String = {
      var str = s.trim.replaceAll(" ", "")
      if (str.length > 0 && str(0) == '(' && FuncUtils.findCloseParen(str, 0) == str.length - 1)
        str = str.substring(1, str.length - 1)
      str
    }

    def trimOperators(): String = {
      var res = s
      if (res(0) == '+') {
        res = res.substring(1)
      }
      if (res(res.length - 1) == '+' || res(res.length - 1) == '-') {
        res = res.substring(0, res.length - 1)
      }
      res
    }
  }

}
