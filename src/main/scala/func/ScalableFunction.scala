package func

/**
  * Created by henri on 11/28/2016.
  */
abstract class ScalableFunction(private var _scalar: BigDecimal) extends Function {

  def scalar: BigDecimal = _scalar

  override def scale(factor: BigDecimal): Unit = {
    _scalar *= factor
  }

  override def scaled(factor: BigDecimal): Function = {
    val scaled = super.clone().asInstanceOf[ScalableFunction]
    scaled.scale(factor)
    scaled
  }

}
