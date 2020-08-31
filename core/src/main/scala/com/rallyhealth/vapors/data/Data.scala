package com.rallyhealth.vapors.data

import scala.language.implicitConversions

/**
  * Magnet type for [[Fact]]s and lifted values.
  */
sealed trait Data[+V] extends Any

object Data {

//  /**
//    * Automatically lift values into data.
//    *
//    * TODO: Is this needed?
//    */
  // This is bad because it wraps ExpAny into a Value, which makes no sense
//  implicit def pure[V](value: V): Value[V] = Value(value)
}

final case class Fact[+V](
  name: String,
  value: V
) extends Data[V]

final case class Value[+A](value: A) extends AnyVal with Data[A]
