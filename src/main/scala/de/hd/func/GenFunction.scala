package de.hd.func

import scala.language.implicitConversions

/**
  * this trait can be created implicitly for a function to generically scale that function
  */
trait GenFunction[T <: Function] {

  def scaled(f: T, scalar: BigDecimal): T

}

object GenFunction {

  implicit class GenFunctionImplicit[T <: Function](f: T) extends GenFunction[T] {
    override def scaled(f: T, scalar: BigDecimal): T = f.scaled(scalar).asInstanceOf[T]
  }

}