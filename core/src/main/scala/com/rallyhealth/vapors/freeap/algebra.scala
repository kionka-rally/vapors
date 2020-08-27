package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyList
import com.rallyhealth.vapors.Data

object algebra {

  sealed trait ExpAlg[+A]

  case class ExpHas[+A](datum: Data[A]) extends ExpAlg[A]

  case class ExpHasAny[+A](data: Seq[Data[A]]) extends ExpAlg[A]

  case class ExpFn[-A, +B](
    data: NonEmptyList[Data[A]] => ExpAlg[B],
    convert: A => B
  ) extends ExpAlg[B]

  case class ExpMap[A, +B](convert: A => B) extends ExpAlg[A]

  case class ExpExists[A](predicate: A => Boolean) extends ExpAlg[A]

  case class ExpAnd[+A](expressions: Seq[ExpAlg[A]]) extends ExpAlg[A]

  case class ExpOr[+A](expressions: Seq[ExpAlg[A]]) extends ExpAlg[A]

  implicit class ExpOps[A](val exp: ExpAlg[A]) extends AnyVal {

    def &&[B >: A](and: ExpAlg[B]): ExpAnd[B] = ExpAnd(List(exp, and))

    def ||[B >: A](orElse: ExpAlg[B]): ExpOr[B] = ExpOr(List(exp, orElse))
  }
}
