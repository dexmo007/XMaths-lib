package de.hd.func.comb

import de.hd.func.exp.ExponentialFunction
import de.hd.func.log.{LnFunction, LogBaseFunction}
import de.hd.func.trig.TrigonometricFunction
import de.hd.func.{Format, Func2Pow, Function, Polynomial, RootFunction, ScalarFunction}

import scala.reflect.ClassTag

/**
  * A product of functions that are composed intelligently and simplified in a list
  */
case class FunctionProduct(private val factors: List[Function]) extends Function {

  override def *(that: Function): FunctionProduct = that.simplified match {
    case product: FunctionProduct =>
      var res = this
      for (f <- product.factors) res *= f
      res
    case trig: TrigonometricFunction =>
      findAndMergeAsPow[TrigonometricFunction](trig, _.getClass)(trig.withScalar(1).asInstanceOf[TrigonometricFunction],
        (t, m) => t.scalar * m.scalar)(_.scalar)
    case exp: ExponentialFunction =>
      simpleFindAndMerge[ExponentialFunction](exp, _.base) {
        (that, matched) => ExponentialFunction(exp.base, that.inner + matched.inner, that.scalar * matched.scalar)
      }
    case log: LogBaseFunction =>
      findAndMergeAsPow[LogBaseFunction](log)(LnFunction(),
        (t, m) => m.scalar * t.scalar / Math.log(t.base.toDouble))(l => l.scalar / Math.log(l.base.toDouble))
    case root: RootFunction =>
      findAndMerge[RootFunction, RootFunction](root) {
        (that, matched) => RootFunction(1 / (1 / that.n + (1 / matched.n)), that.scalar * matched.scalar)
      }(_ => root)
    case poly: Polynomial =>
      simpleFindAndMerge[Polynomial](poly) {
        (that, matched) => that * matched
      }
    case any: Function => FunctionProduct(any :: factors)
  }

  def findAndMerge[T <: Function, M <: Function](that: T, conds: ((T, M) => Boolean)*)
                                                (mergeMatch: (T, M) => Function)
                                                (createNew: T => M)
                                                (implicit evidence: ClassTag[T], ev2: ClassTag[M]): FunctionProduct = {
    val (matched, rest) = Util.genPartition[M](factors, m => conds.isEmpty || conds.forall(eq => eq(that, m)))
    if (matched.size == 1)
      FunctionProduct(mergeMatch(that, matched.head) :: rest)
    else FunctionProduct(createNew(that) :: rest)
  }


  def simpleFindAndMerge[T <: Function](that: T, values: (T => Any)*)
                                       (mergeMatch: (T, T) => Function)(implicit evidence: ClassTag[T]): FunctionProduct = {
    val (matched, rest) = Util.genPartition[T](factors, t => values.isEmpty || values.forall(get => get(that) == get(t)))
    if (matched.size == 1)
      FunctionProduct(mergeMatch(that, matched.head) :: rest)
    else FunctionProduct(that :: rest)
  }

  def findAndMergeAsPow[T <: ScalarFunction](that: T, values: (ScalarFunction => Any)*)
                                            (inner: T, mergeScalar: (T, Func2Pow[T]) => BigDecimal)
                                            (wrappedScalar: T => BigDecimal)
                                            (implicit evidence: ClassTag[T]): FunctionProduct = {
    val (matched, rest) = Util.powPartition[T](factors, t => values.isEmpty || values.forall(get => get(that) == get(t)))
    if (matched.size == 1) {
      val comb = Func2Pow(inner, matched.head.n + 1, mergeScalar(that, matched.head))
      FunctionProduct(comb :: rest)
    } else {
      val comb = Func2Pow(inner, 1, wrappedScalar(that))
      FunctionProduct(comb :: rest)
    }
  }

  override def get(x: BigDecimal): BigDecimal = factors.map(_.get(x)).product

  override def scaled(scalar: BigDecimal): Function = FunctionProduct(factors.head.scaled(scalar) :: factors.tail)

  override protected def derive(): Function =
    factors.head.derivative * FunctionProduct(factors.tail) + (factors.head * FunctionProduct(factors.tail).derive())

  override def antiderive(c: BigDecimal): Function = throw new UnsupportedOperationException

  override protected def getConst: Option[BigDecimal] =
    if (factors.exists(_.const.contains(0))) Some(0)
    else if (factors.forall(_.isConst)) Some(factors.map(_.const.get).product)
    else None

  override def equals(that: Function): Boolean = ???

  // todo only one function
  override protected def simplify: Function = {
    lazy val filtered = factors.filterNot(_.const.contains(1))
    if (isConst) const.get
    else if (filtered.size == 1) filtered.head
    else FunctionProduct(filtered)
  }

  // todo make prettier
  override def stringify(format: Format): String = simplified match {
    case product: FunctionProduct => product.factors.map(f => s"(${f.stringify(format)})").mkString("*")
    case any: Function => any.stringify(format)
  }
}

object FunctionProduct {
  def apply() = new FunctionProduct(List(Function.const(1)))

  def apply(first: Function, second: Function): FunctionProduct = FunctionProduct() * first * second
}
