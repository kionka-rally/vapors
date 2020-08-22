package com.rallyhealth.vapors.freeap

import cats.free.FreeApplicative
import com.rallyhealth.vapors.Data

object algebra {

  sealed trait ExpAlg[A]

  case class ExpHas[A](value: Data[A]) extends ExpAlg[A]

  case class ExpAnd[A](expressions: Seq[FreeApplicative[ExpAlg, A]]) extends ExpAlg[A]

  case class ExpOr[A](expressions: Seq[FreeApplicative[ExpAlg, A]]) extends ExpAlg[A]
}
