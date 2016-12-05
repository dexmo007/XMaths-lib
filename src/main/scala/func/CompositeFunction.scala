package func

/**
  * A collection of functions to be summed up, mapped by their class
  */
case class CompositeFunction private[func](private var addends: Map[Class[_ <: Function], List[Function]]) extends Function {

  def add(that: Function): Unit = {
    val sub = addends.getOrElse(that.getClass, Nil)
    addends += (that.getClass -> (that :: sub))
  }

  override def get(x: BigDecimal): BigDecimal = addends.values.map(_.map(_.get(x)).sum).sum

  override def scale(factor: BigDecimal): Unit = addends.values.foreach(_.foreach(_.scale(factor)))

  override def scaled(factor: BigDecimal): Function = CompositeFunction(addends.mapValues(_.map(_ * factor)))

  override def derive(): Function = CompositeFunction(addends.mapValues(_.map(_.derive())))

  override def antiderive(c: BigDecimal): Function = CompositeFunction(addends.mapValues(_.map(_.antiderive(c))))

  override def constValue: Option[BigDecimal] = {
    var totalConst: BigDecimal = 0
    for (f <- addends.values.flatten.map(_.constValue)) {
      if (f.isDefined)
        totalConst += f.get
      else
        return None
    }
    Some(totalConst)
  }

  override def stringify(format: Format): String = ???

}

object CompositeFunction {
  def apply(): CompositeFunction = new CompositeFunction(Map.empty)
}
