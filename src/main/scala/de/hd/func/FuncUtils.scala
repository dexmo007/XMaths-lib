package de.hd.func

import de.hd.func.FunctionParser.FunctionParseException
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
