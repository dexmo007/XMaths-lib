package de.hd.func.impl2.pow

import de.hd.func.impl2.{MathFunction, Polynomial}

/**
  * A function that takes another function `g` and results in the power to an integer value `i`:
  * f(x) = g(x)&#94;n, where n is an integer
  *
  * @author Henrik Drefs
  */
case class Func2Pow(override val g: MathFunction, i: Int) extends AnyFunc2Pow(g, i) {

  override def apply(x: BigDecimal): BigDecimal = g(x) pow i

  /**
    * @return derivative in form of f(x) = g(x)&#94;n --> f'(x) = n * g'(x) * g(x)&#94;(n-1)
    */
  override protected def derive(): MathFunction = g.derivative * (g pow (i - 1)) * i

  /**
    * @return anti-derivative if inner is linear: f(x) = g(x)&#94;n with g(x) = ax + b
    *         --> F(x) = (ax+b)&#94;(n+1) / ((n +1 ) * a) + c
    * @throws UnsupportedOperationException if inner function is not linear
    */
  override def antiDerive(c: BigDecimal): MathFunction =
    if (!g.isLinear) throw new UnsupportedOperationException
    else if (i == -1) MathFunction.ln(g) / g.gradient
    else (g pow (i + 1)) / ((i + 1) * g.gradient) + c

  override def pow(n: Int): MathFunction = Func2Pow(g, this.i * n)

  override protected def getConst: Option[BigDecimal] =
    if (i == 0) Some(1)
    else if (g.isConst) g.const.map(_ pow i)
    else None

  override protected def simplify: MathFunction =
    if (isConst) const.get
    else if (i == 1) g.simplified
    // if f(x) = (ax)^n then a^n * x^n
    else if (g.isLinear && g(0) == 0) Polynomial(i -> (g.gradient pow i))
    else g.simplified match {
      // todo replace _,_ with this.n
      //      case RootFunction(_, _) => f.simplified
      case _ => copy(g = g.simplified)
    }
}
