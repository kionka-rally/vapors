package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyVector
import cats.free.FreeApplicative

object dsl {
  import algebra._

  type ExpDsl[T, A] = FreeApplicative[ExpAlg[T, *], A]

  private def lift[T, A](value: ExpAlg[T, A]): ExpDsl[T, A] = FreeApplicative.lift(value)

  def has[T](predicate: T => Boolean): ExpDsl[T, Boolean] = lift(ExpHas(predicate))

  def and[T](expressions: NonEmptyVector[ExpDsl[T, Boolean]]): ExpDsl[T, Boolean] =
    lift(
      ExpAnd[T, Boolean](
        _.foldLeft(true)(_ && _),
        expressions
      )
    )

  def or[T](expressions: NonEmptyVector[ExpDsl[T, Boolean]]): ExpDsl[T, Boolean] =
    lift(
      ExpOr[T, Boolean](
        _.foldLeft(false)(_ || _),
        expressions
      )
    )
}
