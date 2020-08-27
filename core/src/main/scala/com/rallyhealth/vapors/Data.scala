package com.rallyhealth.vapors

import scala.language.implicitConversions

/**
  * Magnet type for [[Fact]]s and lifted values.
  */
sealed trait Data[+V] extends Any

object Data {

  /**
    * Automatically lift values into data.
    *
    * TODO: Is this needed?
    */
  implicit def pure[V](value: V): Value[V] = Value(value)
}

final case class Fact[+V](
  name: String,
  value: V
) extends Data[V]

final case class Value[+A](value: A) extends AnyVal with Data[A]
