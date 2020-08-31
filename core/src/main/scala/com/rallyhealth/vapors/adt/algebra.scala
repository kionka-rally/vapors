package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.data.{Data, Fact, Window}

import scala.reflect.ClassTag

object algebra {

  sealed trait AlgExp[A]

  final case class HasExp[A](value: Data[A]) extends AlgExp[A]

  final case class HasAnyExp[A](dataset: Set[Data[A]]) extends AlgExp[A]

  final case class WithNameExp[A](names: Set[String], exp: AlgExp[A]) extends AlgExp[A]

  final case class WithTypeExp[A, X <: A](exp: AlgExp[X])(implicit ct: ClassTag[X]) extends AlgExp[A] {
    type Sub = X

    def cast(fact: Fact[A]): Option[Fact[Sub]] = fact match {
      case f: Fact[X] if ct.unapply(f.value).isDefined => Some(f)
      case _ => None
    }

    def subClassTag: ClassTag[Sub] = ct

    def subExp: AlgExp[Sub] = exp

    // TODO: Is this needed?
    /**
      * Widens the free parameter to the bottom type [[Any]].
      *
      * [[ClassTag]]s do not require any specific type. The expression lower bound is only defined
      * by this class to avoid dropping the lower-bound to Any whenever present in a list of expressions.
      * This only exists because I can't make the type parameter covariant.
      */
    def widen: WithTypeExp[Any, X] = this.asInstanceOf[WithTypeExp[Any, X]]
  }

  final case class WithinWindowExp[A](range: Window[A]) extends AlgExp[A]

  final case class AndExp[A] private (expressions: Vector[AlgExp[A]]) extends AlgExp[A]

  final object AndExp {
    def apply[A](expressions: AlgExp[A]*): AndExp[A] = new AndExp(expressions.toVector)
    def fromVector[A](expressions: Vector[AlgExp[A]]): AndExp[A] = new AndExp(expressions)
  }

  final case class OrExp[A] private (expressions: Vector[AlgExp[A]]) extends AlgExp[A]

  final object OrExp {
    def apply[A](expressions: AlgExp[A]*): OrExp[A] = new OrExp(expressions.toVector)
    def fromVector[A](expressions: Vector[AlgExp[A]]): OrExp[A] = new OrExp(expressions)
  }
}
