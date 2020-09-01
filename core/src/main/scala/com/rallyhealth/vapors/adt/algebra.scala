package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.data.{Data, Fact, FactTypeSet, Result, Window}

import scala.reflect.ClassTag

object algebra {

  sealed trait AlgExp[A]

  final case class FailedExp[A]() extends AlgExp[A]

  final case class HasAnyExp[A](dataset: Set[Data[A]]) extends AlgExp[A]

  final case class WithFactTypesExp[A <: B, B : ClassTag](
    factTypes: FactTypeSet[A],
    subExp: AlgExp[A]
  ) extends AlgExp[B] {

    def widen(result: Result[_ <: A]): Result[B] = {
      Result.fromList(result.matchingFacts.flatMap(factTypes.getAs[B](_)))
    }
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
