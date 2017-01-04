package de.hd.func.impl2

import scala.reflect.ClassTag

/**
  * Created by henri on 12/27/2016.
  */
trait Addable[T <: SelfScaled[T]] extends SelfScaled[T] {
  this: T =>


}
