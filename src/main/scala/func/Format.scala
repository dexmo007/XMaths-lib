package func

import func.trig.TrigonometricFunction
import func.FuncUtils._
import org.apache.commons.math3.fraction.Fraction

/**
  * Trait that defines formatting options
  */
trait Format {

  val pi: String

  def fraction(fraction: Fraction): String

  def num(n: BigDecimal): String = {
    if (n.isValidInt) n.toInt.toString
    else if (n % Math.PI == 0) s"${scalar(n / Math.PI)}$pi"
    else if (n % Math.E == 0) s"${scalar(n / Math.E)}e"
    else {
      val fraction = new Fraction(n.toDouble, 1e-15, 100)
      if (fraction.getDenominator > 1000)
        n.toString
      else if ((fraction.getNumerator.toDouble / fraction.getDenominator).isValidInt)
        (fraction.getNumerator.toDouble / fraction.getDenominator).toInt.toString
      else
        this.fraction(fraction)
    }
  }

  def scalar(scalar: BigDecimal): String

  def root(n: BigDecimal): String

  def base(base: BigDecimal): String = {
    val s = num(base)
    if (s.startsWith("-") || s.contains("/") || s.contains("*"))
      s.wrap
    else
      s
  }

  def base(f: Function): String = {
    if (f.isConst)
      base(f.constValue.get)
    else f match {
      case trig: TrigonometricFunction =>
        if (trig.scalar == 1)
          trig.stringify(this)
        else
          trig.stringify(this).wrap
      case _ => f.stringify(this).wrap
    }
  }

  def pow(s: String): String

  def pow(f: Function): String = {
    if (f.isConst && f.constValue.get == 1)
      return ""
    var s = f.stringify(this)
    if (s.length > 1 && !s.containsNoOps)
      s = s.wrap
    pow(s)
  }

  def pow(n: BigDecimal): String = {
    if (n == 1)
      return ""
    var s: String = num(n)
    if (s.contains("/") || s.contains("/"))
      s = s.wrap
    pow(s)
  }

}

/**
  * Provides (statically) some default Format implementations
  */
object Format {
  /**
    * formats in TeX-notation
    */
  val Tex = new Format {

    override val pi: String = "\\pi"

    override def fraction(fraction: Fraction): String = s"\\frac{${fraction.getNumerator}}{${fraction.getDenominator}}"

    override def scalar(scalar: BigDecimal): String = {
      if (scalar == 1) ""
      else if (scalar == -1) "-"
      else num(scalar)
    }

    override def root(n: BigDecimal): String = {
      if (n == 1) "x"
      else if (n == 2) "\\sqrt{x}"
      else s"\\sqrt[${num(n)}]{x}"
    }

    override def pow(s: String): String = s"^{$s}"
  }
  /**
    * formats as readable plain string
    */
  val Plain = new Format {

    override val pi: String = "pi"

    override def fraction(fraction: Fraction): String = fraction.toString.replaceAll(" ", "")

    override def scalar(scalar: BigDecimal): String = {
      if (scalar == 1) ""
      else if (scalar == -1) "-"
      else s"${num(scalar)}*"
    }

    override def root(n: BigDecimal): String = {
      if (n == 1) "x"
      else if (n == 2) "sqrt(x)"
      else if (n == 3) "cbrt(x)"
      else s"x${pow(1 / n)}"
    }

    override def pow(s: String): String = s"^$s"

  }

}
