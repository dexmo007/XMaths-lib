package func

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import func.FuncUtils.FuncString

/**
  * Created by Henrik on 7/8/2016.
  */
object FunctionParser {
  private val validChars = "+-*^x."
  private val validOps = "+-*/^"

  /**
    * Tries to parse string to function, if success, that function is returned, else evaluation is tried, if success, the value is returned, else Exception is thrown
    *
    * @param s string to be parsed or evaluated
    * @return Function or BigDecimal
    */
  def parseOrEval(s: String): Any = {
    try {
      parse(s)
    } catch {
      case _: Exception =>
        try {
          evaluateSingle(s)
        } catch {
          case _: Exception =>
            throw FunctionParseException()
        }
    }
  }

  //todo rm?
  def evaluateParen(s: String): BigDecimal = {
    // ValidateParentheses , only need once actually
    val openPar = s.indexOf("(")
    if (openPar != -1) {
      val closePar = FuncUtils.findCloseParen(s, openPar)
    }
    0
  }

  def evaluate2(s: String): BigDecimal = {
    // if s has no operators
    if (validOps.forall(op => !s.contains(op))) {
      evaluateSingle(s)
    }
    val ops = new Array[Char](0).toBuffer
    val terms = new Array[Any](0).toBuffer
    var lastOpIndex = -1
    var i = 0
    while (i < s.length) {
      if (s(i) == '(') {
        i = FuncUtils.findCloseParen(s, i)
      } else if (validOps.contains(s(i))) {
        terms.append(s.substring(lastOpIndex + 1, i).trimBraces())
        ops.append(s(i))
        lastOpIndex = i
      }
      i += 1
    }
    terms.append(s.substring(lastOpIndex + 1, s.length).trimBraces())
    perform(ops, terms, "^")
    perform(ops, terms, "*/")
    perform(ops, terms, "+-")
    terms.head match {
      case d: BigDecimal => d
      case s: String => evaluate2(s)
    }
  }

  def perform(ops: mutable.Buffer[Char], terms: mutable.Buffer[Any], op: String): Unit = {
    var i = 0
    while (i < ops.length) {
      if (op.contains(ops(i))) {
        val term1 = terms(i) match {
          case s: String => evaluate2(s)
          case d: BigDecimal => d
        }
        val term2 = terms(i + 1) match {
          case s: String => evaluate2(s)
          case d: BigDecimal => d
        }
        terms(i) = getOperation(ops(i))(term1, term2)
        ops.remove(i)
        terms.remove(i + 1)
      } else {
        i += 1
      }
    }
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
    } else {
      try {
        BigDecimal(s)
      } catch {
        case _: Exception => throw new IllegalArgumentException("String invalid!")
      }
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

  private def parseAddend(addend: String): Function = {
    val s = addend.unwrap
    var lastOpIndex = -1
    val ops = new ListBuffer[Char]()
    val factors = new ListBuffer[Function]()
    var i = 0
    while (i < s.length) {
      val c = s(i)
      if (c == '/' || c == '*') {
        ops += c
        // check if previous factor is pure number
        val option = tryParseNumber(s.substring(lastOpIndex + 1, i))
        if (option.isDefined) factors += option.get
        lastOpIndex = i
      } else if (c.isLetter) {
        var j = i + 1
        while (j < s.length && s(j).isLetter)
          j += 1
        // skip these letters in loop
        val advance = j - i - 1
        val func: Function = s.substring(i, j) match {
          case "e" => Function.const(Math.E)
          case "pi" => Function.const(Math.PI)
          case "x" =>
            val scalar = readScalar(s, i - 1)
            val pow = readPow(s, i + 1)
            Function.xToN(pow, scalar)
          // must be an inner function
          case any: String =>
            if (s(j) != '(')
              throw FunctionParseException("inner function identifier not followed by (...)")
            val close = FuncUtils.findCloseParen(s, j)
            val scalar = readScalar(s, i - 1)
            // skip (...) for loop
            i += close - j
            getFunction(any)(scalar) of parse(s.substring(j + 1, close))
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

  private def add(f1: Function, f2: Function): Function = {
    (f1, f2) match {
      case (p1: Polynomial, p2: Polynomial) =>
        val reduced = p1.scalars.zip(p2.scalars).map(x => x._1 + x._2)
        Polynomial(reduced: _*)
      case _ =>
        f1 + f2
    }
  }

  def parse(s: String): Function = {
    val str = s.loseRedundant
    var res: Function = FunctionsSum()
    for (rawAddend <- str.splitAddends) {
      // todo maybe simplify each addend?
      res += parseAddend(rawAddend).simplified
    }
    res.simplified
  }


  /**
    *
    * parser for string like "3*x&#94;2+4x+5"
    *
    * @param s to be parsed
    * @return
    */
  def parsePolynomial(s: String): Polynomial = {
    //    checkString(s)
    var map = mutable.Map[Int, BigDecimal]()
    // iterates thru all summands that scale x^n
    for (i <- s.indices) {
      if (s(i) == 'x') {
        val pow = readPow(s, i + 1)
        val scalar = readScalar(s, i - 1)
        if (map.contains(pow)) {
          map(pow) += scalar
        } else {
          map += pow -> scalar
        }
      }
    }
    // get scale of x^0
    map += 0 -> getSimpleSummand(s)

    val level = map.keysIterator.max
    val scls: Array[BigDecimal] = Array.fill(level + 1) {
      0.0
    }
    map.foreach(entry => scls(entry._1) = entry._2)
    Function.polynomial(scls: _*).asInstanceOf[Polynomial]
  }

  def getSimpleSummand(s: String): BigDecimal = {
    var sum: BigDecimal = 0
    for (str <- s.split('+')) {
      if (!str.isEmpty && !str.contains('x')) {
        //        println(str)
        sum += BigDecimal(str.trim)
      } else if (str.contains('-')) {
        val split = str.split('-')
        if (!split(0).contains('x')) {
          sum += BigDecimal(split(0))
        }
        for (i <- 1 until split.length) {
          if (!split(i).contains('x')) {
            sum -= BigDecimal(split(i))
          }
        }
      }
    }
    sum
  }

  // todo rm
  def checkString(s: String): Unit = {
    // todo incomplete
    for (c <- s) {
      if (!validChars.contains(c) && !c.isDigit) {
        throw FunctionParseException("String contains invalid character: " + c)
      }
    }
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
  def getFunction(symbol: String): (BigDecimal) => Function = {
    val s = symbol.toLowerCase
    s match {
      case "ln" => (scale: BigDecimal) => Function.ln(scale)
      case "exp" => (scale: BigDecimal) => Function.exp(scale)
      case "sin" => (scale: BigDecimal) => Function.sin(scale)
      case "asin" => (scale: BigDecimal) => Function.asin(scale)
      case "cos" => (scale: BigDecimal) => Function.cos(scale)
      case "acos" => (scale: BigDecimal) => Function.acos(scale)
      case "tan" => (scale: BigDecimal) => Function.tan(scale)
      case "atan" => (scale: BigDecimal) => Function.atan(scale)
      case "cot" => (scale: BigDecimal) => Function.cot(scale)
      case "acot" => (scale: BigDecimal) => Function.acot(scale)
      case _ => throw FunctionParseException("No such function:" + symbol)
    }
  }

  case class FunctionParseException(message: String = "") extends FunctionException(message)

  def main(args: Array[String]) {
    println(parseAddend("pi*sin(x)"))
  }

}
