package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyVector
import cats.free.FreeApplicative

// set of inferences
class BigData
object BigData

object algebra {

  sealed trait ExpAlg[A]

  case class ExpHas[A](predicate: BigData => A) extends ExpAlg[A]

  case class ExpAnd[A](predicate: NonEmptyVector[A] => A, expressions: NonEmptyVector[FreeApplicative[ExpAlg, A]]) extends ExpAlg[A]

  case class ExpOr[A](predicate: NonEmptyVector[A] => A, expressions: NonEmptyVector[FreeApplicative[ExpAlg, A]]) extends ExpAlg[A]
}
