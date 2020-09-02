package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyVector
import cats.free.FreeApplicative

object dsl {
  import algebra._

  type ExpDsl[A] = FreeApplicative[ExpAlg, A]

  private def lift[A](value: ExpAlg[A]): ExpDsl[A] = FreeApplicative.lift(value)

  def has(predicate: BigData => Boolean): ExpDsl[Boolean] = lift(ExpHas(predicate))

  def and(expressions: NonEmptyVector[ExpDsl[Boolean]]): ExpDsl[Boolean] =
    lift(ExpAnd[Boolean](_.foldLeft(true)(_ && _), expressions))

  def or(expressions: NonEmptyVector[ExpDsl[Boolean]]): ExpDsl[Boolean] =
    lift(ExpOr[Boolean](_.foldLeft(false)(_ || _), expressions))
}
