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

  //  override def *(that: Function): Function = that.simplified match {
  //    case trig: TrigonometricFunction =>
  //      val (matched, rest) = Util.genPartition[TrigonometricFunction](factors, _.getClass == trig.getClass)
  //      if (matched.size == 1) {
  //        // found a matching trig function
  //        val comb = Func2Pow(matched.head.withScalar(1).asInstanceOf[TrigonometricFunction], 2, matched.head.scalar * trig.scalar)
  //        FunctionProduct(comb :: rest)
  //      } else {
  //        val (matched, rest) = Util.genPartition[Func2Pow[TrigonometricFunction]](factors, _.inner.getClass == trig.getClass)
  //        if (matched.size == 1) {
  //          // found a matching trig function to the nth power
  //          val comb = Func2Pow(trig.withScalar(1).asInstanceOf[TrigonometricFunction], matched.head.n + 1, trig.scalar * matched.head.scalar)
  //          FunctionProduct(comb :: rest)
  //        } else FunctionProduct(trig :: rest)
  //      }
  //    case exp: ExponentialFunction =>
  //      val (matched, rest) = Util.genPartition[ExponentialFunction](factors, _.base == exp.base)
  //      if (matched.size == 1) {
  //        val comb = ExponentialFunction(exp.base, matched.head.inner + exp.inner, exp.scalar * matched.head.scalar)
  //        FunctionProduct(comb :: rest)
  //      } else FunctionProduct(exp :: rest)
  //    case log: LogBaseFunction =>
  //      val (matched, rest) = Util.genPartition[Func2Pow[LnFunction]](factors, _ => true)
  //      if (matched.size == 1) {
  //        val comb = Func2Pow(LnFunction(), matched.head.n + 1, matched.head.scalar * log.scalar / Math.log(log.base.toDouble))
  //        FunctionProduct(comb :: rest)
  //      } else {
  //        val mut = Func2Pow(LnFunction(), 1, log.scalar / log.base)
  //        FunctionProduct(mut :: rest)
  //      }
  //    case _: Function => super.*(that)
  //  }

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
