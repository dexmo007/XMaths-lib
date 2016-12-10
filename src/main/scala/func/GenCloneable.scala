package func

/**
  * trait that enables generic clone support
  */
trait GenCloneable[+T] extends Cloneable {

  def cloned(): T = clone().asInstanceOf[T]

  override def clone(): AnyRef = super.clone()

}
