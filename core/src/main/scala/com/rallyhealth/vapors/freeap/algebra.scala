package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyVector
import cats.free.FreeApplicative

object algebra {

  sealed trait ExpAlg[T, A]

  case class ExpHas[T, A](predicate: T => A) extends ExpAlg[T, A]

  case class ExpAnd[T, A](predicate: NonEmptyVector[A] => A, expressions: NonEmptyVector[FreeApplicative[ExpAlg[T, *], A]]) extends ExpAlg[T, A]

  case class ExpOr[T, A](predicate: NonEmptyVector[A] => A, expressions: NonEmptyVector[FreeApplicative[ExpAlg[T, *], A]]) extends ExpAlg[T, A]
}
