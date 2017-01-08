package de.hd.func

import de.hd.func.FuncUtils.FuncString
import de.hd.func.impl2.op.FunctionSum
import de.hd.func.impl2.{MathFunction, Polynomial}

import scala.collection.mutable.ListBuffer

/**
  * Created by Henrik on 7/8/2016.
  */
object FunctionParser {
  private val validChars = "+-*^x."
  private val validOps = "+-*/^"

  def tryParse(s: String): Option[MathFunction] = try {
    Some(parse(s))
  } catch {
    case _: Exception => None
  }

  def parseOrEval(s: String): Either[MathFunction, BigDecimal] = {
    val f = parse(s)
    if (f.isConst)
      Right(f.const.get)
    else Left(f)
  }

  def tryParseOrEval(s: String): Option[Either[MathFunction, BigDecimal]] = try {
    Some(parseOrEval(s))
  } catch {
    case _: Exception => None
  }

  implicit class BraceString(s: String) {
    def trimBraces(): String = {
      var res = s
      if (res.head == '(') {
        res = res.substring(1)
      }
      if (res.last == ')') {
        res = res.substring(0, res.length - 1)
      }
      res
    }
  }

  def getOperation(c: Char): (BigDecimal, BigDecimal) => BigDecimal = {
    c match {
      case '+' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 + bd2
      case '-' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 - bd2
      case '*' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 * bd2
      case '/' =>
        (bd1: BigDecimal, bd2: BigDecimal) => bd1 / bd2
      case '^' =>
        (bd1: BigDecimal, bd2: BigDecimal) => Math.pow(bd1.toDouble, bd2.toDouble)
      case _ =>
        throw FunctionParseException("Invalid character: " + c)
    }
  }

  /**
    * @return the value of the string as big decimal
    * @throws IllegalArgumentException if string is not a single number or none of (e, pi)
    */
  def evaluateSingle(s: String): BigDecimal = {
    if (s == "e" || s == "E") {
      Math.E
    } else if (s == "pi" || s == "Pi" || s == "PI") {
      Math.PI
    } else if (s.isEmpty) {
      0
    } else try {
      BigDecimal(s)
    } catch {
      case _: Exception => throw new IllegalArgumentException("String invalid!")
    }
  }

  def tryParseNumber(s: String): Option[BigDecimal] = try {
    Some(BigDecimal(s))
  } catch {
    case _: Exception =>
      None
  }

  implicit class Parsable(string: String) {
    def loseRedundant: String = {
      var str = string.trim.replaceAll(" ", "")
      // check for y = .. or f(x) = .. declaration
      if (str.contains("=")) {
        val split = str.split("=")
        if (split.length != 2 || (split(0) != "y" && split(0) != "f(x)"))
          throw FunctionParseException()
        str = split(1)
      }
      str
    }


    def splitAddends: List[String] = {
      val plus = '+'
      var off = 0
      val s = string.replaceAll("-", "+-")
      var next = s.indexOf(plus, off)
      val list = new ListBuffer[String]
      while (next != -1) {
        val substring = s.substring(off, next)
        if (substring.contains("(") && !substring.contains(")")) {
          next = s.indexOf(plus, next + 1)
        } else {
          list += substring
          off = next + 1
          next = s.indexOf(plus, off)
        }
      }
      // If no match was found, return this
      if (off == 0) return List(s)

      // add remaining segment
      list += s.substring(off, s.length())
      list.toList
    }
  }

  private def parseAddend(addend: String): MathFunction = {
    val s = addend.unwrap
    var lastOpIndex = -1
    val ops = new ListBuffer[Char]()
    val factors = new ListBuffer[MathFunction]()
    var i = 0
    while (i < s.length) {
      val c = s(i)
      if (c == '/' || c == '*') {
        ops += c
        // check if previous factor is pure number
        val option = tryParseNumber(s.substring(lastOpIndex + 1, i))
        if (option.isDefined) factors += MathFunction.const(option.get)
        lastOpIndex = i
      } else if (c.isLetter) {
        var j = i + 1
        while (j < s.length && s(j).isLetter)
          j += 1
        // skip these letters in loop
        val advance = j - i - 1
        val func: MathFunction = s.substring(i, j) match {
          case "e" => MathFunction.const(Math.E)
          case "pi" => MathFunction.const(Math.PI)
          case "x" =>
            val scalar = readScalar(s, i - 1)
            val pow = readPow(s, i + 1)
            Polynomial(pow -> scalar)
          // must be an inner function
          case any: String =>
            if (s(j) != '(')
              throw FunctionParseException("inner function identifier not followed by (...)")
            val close = FuncUtils.findCloseParen(s, j)
            val scalar = readScalar(s, i - 1)
            // skip (...) for loop
            i += close - j
            evaluateFunction(any)(parse(s.substring(j + 1, close))) * scalar
        }
        i += advance
        factors += func
      }
      i += 1
    }
    var reduced = factors.head
    for (i <- ops.indices) {
      ops(i) match {
        case '/' => reduced /= factors(i + 1)
        case '*' => reduced *= factors(i + 1)
      }
    }
    reduced
  }

  def parse(s: String): MathFunction = {
    val str = s.loseRedundant
    var res: MathFunction = FunctionSum()
    for (rawAddend <- str.splitAddends) {
      res += parseAddend(rawAddend).simplified
    }
    res.simplified
  }

  def readPow(s: String, index: Int): Int = {
    if (index >= s.length || s(index) != '^') {
      return 1
    }
    var i = index + 1
    val sb = StringBuilder.newBuilder
    while (i < s.length && s(i).isDigit) {
      sb += s(i)
      i += 1
    }
    sb.toInt
  }

  def readScalar(s: String, index: Int): BigDecimal = {
    var i = index
    // no scalar = 1 v -1
    if (i < 0 || s(i) == '+' || s(i) == '*' || s(i) == '/') {
      return 1
    }
    if (s(i) == '-') {
      return -1
    }
    // multiply sign
    if (s(i) == '*') {
      i -= 1
    }
    val sb = StringBuilder.newBuilder
    // read all digits
    while (i >= 0 && (s(i).isDigit || s(i) == '.')) {
      sb += s(i)
      i -= 1
    }
    val num = BigDecimal(sb.reverse.toString)
    // determine sign
    if (i >= 0 && s(i) == '-') {
      -num
    } else {
      num
    }
  }

  // todo as map
  //  def getFunction(symbol: String): (BigDecimal) => Function = {
  //    val s = symbol.toLowerCase
  //    s match {
  //      case "ln" => (scalar: BigDecimal) => Function.ln(scalar)
  //      case "exp" => (scalar: BigDecimal) => Function.exp(scalar = scalar)
  //      case "sin" => (scalar: BigDecimal) => Function.sin(scalar)
  //      case "asin" => (scalar: BigDecimal) => Function.asin(scalar)
  //      case "cos" => (scalar: BigDecimal) => Function.cos(scalar)
  //      case "acos" => (scalar: BigDecimal) => Function.acos(scalar)
  //      case "tan" => (scalar: BigDecimal) => Function.tan(scalar)
  //      case "atan" => (scalar: BigDecimal) => Function.atan(scalar)
  //      case "cot" => (scalar: BigDecimal) => Function.cot(scalar)
  //      case "acot" => (scalar: BigDecimal) => Function.acot(scalar)
  //      case _ => throw FunctionParseException("No such function:" + symbol)
  //    }
  //  }

  private val functionsMap: Map[String, (MathFunction => MathFunction)] = Map(
    "ln" -> MathFunction.ln,
    "log" -> MathFunction.ln,
    "exp" -> MathFunction.exp,
    "sin" -> MathFunction.sin,
    "cos" -> MathFunction.cos,
    "tan" -> MathFunction.tan
  )

  def evaluateFunction(symbol: String): (MathFunction => MathFunction) = {
    if (functionsMap.contains(symbol))
      functionsMap(symbol)
    else if (symbol startsWith "log") {
      val base = BigDecimal(symbol.substring(3))
      if (base == 10) MathFunction.log10
      else f => MathFunction.logb(base, f)
    }
    else throw FunctionParseException("No such function: " + symbol)
  }

  case class FunctionParseException(message: String = "") extends FunctionException(message)

  def main(args: Array[String]) {
    println(parseAddend("pi*sin(x)"))
  }

}
