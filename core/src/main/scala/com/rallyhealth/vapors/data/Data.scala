package com.rallyhealth.vapors.data

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
  * Magnet type for [[Fact]]s and lifted values.
  */
sealed trait Data[V] extends Any

final case class Fact[V](
  typeInfo: FactType[V],
  value: V
) extends Data[V]

final case class Value[A](value: A) extends AnyVal with Data[A]

trait FactType[V] {
  def name: String
  def classTag: ClassTag[V]
}

object FactType {

  def apply[V : ClassTag](name: String): FactType[V] = Simple(name)

  private final case class Simple[V](
    name: String
  )(implicit
    override val classTag: ClassTag[V]
  ) extends FactType[V]
}
