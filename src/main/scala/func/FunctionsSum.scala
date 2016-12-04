package func

/**
  * Created by henri on 12/5/2016.
  */
case class FunctionsSum private[func](private val addends: List[Function]) extends Function {

  override def +(that: Function): Function = FunctionsSum(that :: addends)

  override def get(x: BigDecimal): BigDecimal = addends.reduce((f1, f2) => f1.get(x) + f2.get(x))

  override def scale(factor: BigDecimal): Unit = for (i <- addends.indices) addends(i) *= factor

  override def scaled(factor: BigDecimal): Function = FunctionsSum(addends.map(_ * factor))

  override def derive(): Function = FunctionsSum(addends.map(_.derive()))

  override def antiderive(c: BigDecimal): Function = FunctionsSum(addends.map(_.antiderive(c)))

  override def constValue: Option[BigDecimal] = {
    var totalConst = 0
    for (f <- addends) {
      val const = f.constValue
      if (const.isDefined)
        totalConst += const.get
      else
        return None
    }
    Some(totalConst)
  }

  // todo impl
  override def stringify(format: Format): String = ???
}
