package com.rallyhealth.vapors.data

import cats.Functor

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

/**
  * Magnet type for [[Fact]]s and lifted values.
  */
sealed trait Data[V] {
  def isValueOnly: Boolean
}

final case class Fact[V](
  typeInfo: FactType[V],
  value: V
) extends Data[V] {
  override def isValueOnly: Boolean = false
}

final case class FactTypeSet[A] private (types: Map[String, FactType[A]]) {

  /**
    * Checks the [[FactType]] for equality with one of the types in this set.
    *
    * @return the [[Fact]] as the expected type.
    */
  def getAs[B : ClassTag](fact: Fact[_]): Option[Fact[B]] = fact match {
    case Fact(tpe, _: B) if types.get(tpe.fullName).contains(tpe) =>
      // Justification: This checks equality of the FactType at runtime after safely casting the value
      Some(fact.asInstanceOf[Fact[B]])
    case _ => None
  }

  def subset[B](implicit tb: TypeTag[B]): Option[FactTypeSet[B]] = {
    val matchingFactTypes = types.toList.collect {
      case (_, factType) if factType.tt.tpe =:= tb.tpe =>
        // Justification: This checks equality of the FactType at runtime after safely casting the value
        factType.asInstanceOf[FactType[B]]
    }
    FactTypeSet.fromList(matchingFactTypes)
  }

  final object Match {

    def apply(fact: Fact[_]): Option[Fact[A]] = {
      types.get(fact.typeInfo.name).flatMap(_.cast(fact))
    }

    def unapply(fact: Fact[_]): Option[Fact[A]] = apply(fact)
  }
}

object FactTypeSet {

  def empty[A]: FactTypeSet[A] = new FactTypeSet(Map())

//  sealed trait TaggedFactType[A] {
//    def tpe: FactType[A]
//    def tag: ClassTag[A]
//  }
//
//  private final class TaggedFactTypeImpl[A](override val tpe: FactType[A])(implicit override val tag: ClassTag[A])
//    extends TaggedFactType[A]
//
//  implicit def tagFactType[A : ClassTag](factType: FactType[A]): TaggedFactType[A] = new TaggedFactTypeImpl(factType)

  def fromSingle[A](one: FactType[A]): FactTypeSet[A] = {
    new FactTypeSet(Map(one.fullName -> one))
  }

  def fromSet[A](types: Set[FactType[A]]): Option[FactTypeSet[A]] = {
    fromList(types.toList)
  }

  def fromList[A](types: List[FactType[A]]): Option[FactTypeSet[A]] = {
    val typeMap = types.groupBy(_.fullName).collect {
      case (name, tpe :: Nil) => (name, tpe)
    }
    if (typeMap.size != types.size) None
    else Some(new FactTypeSet(typeMap))
  }
}

final case class Value[A](value: A) extends Data[A] {
  override def isValueOnly: Boolean = true
}

object Value {
  implicit object FunctorImpl extends Functor[Value] {
    override def map[A, B](fa: Value[A])(f: A => B): Value[B] = Value(f(fa.value))
  }
}

trait FactType[V] {

  def name: String

  final lazy val fullName: String = s"$name: ${tt.tpe}"

  protected[vapors] implicit val ct: ClassTag[V]
  protected[vapors] implicit val tt: TypeTag[V]

  /**
    * Safely cast the given fact to this type.
    */
  def cast(fact: Fact[_]): Option[Fact[V]] = {
    fact match {
      // Justification: This checks equality of the FactType after safely casting the fact value
      case f @ Fact(tpe, _: V) if tpe == this => Some(f.asInstanceOf[Fact[V]])
      case _ => None
    }
  }

  /**
    * Defines FactType equality as "both types have the same name and compiled type."
    */
  override final def equals(o: Any): Boolean = o match {
    case that: FactType[_] =>
      this.fullName == that.fullName && this.tt.tpe =:= that.tt.tpe
    case _ => false
  }

  override final def hashCode: Int = this.fullName.hashCode()
}

object FactType {

  def apply[V : ClassTag : TypeTag](name: String): FactType[V] = Simple(name)

  private final case class Simple[V](
    name: String
  )(implicit
    override val ct: ClassTag[V],
    override val tt: TypeTag[V]
  ) extends FactType[V]
}
