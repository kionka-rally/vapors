package com.rallyhealth.vapors.freeap

import cats.free.FreeApplicative
import com.rallyhealth.vapors.Data

object dsl {
  import algebra._

  type ExpDsl[A] = FreeApplicative[ExpAlg, A]

  private def lift[A](value: ExpAlg[A]): ExpDsl[A] = FreeApplicative.lift(value)

  def has[A](exp: Data[A]): ExpDsl[A] = lift(ExpHas(exp))

  def and[A](expressions: ExpDsl[A]*): ExpDsl[A] = lift(ExpAnd(expressions))

  implicit class DslOps[A](val op: ExpDsl[A]) extends AnyVal {

    def &&(andThen: ExpDsl[A]): ExpDsl[A] = lift(ExpAnd(Seq(op, andThen)))

    def ||(orElse: ExpDsl[A]): ExpDsl[A] = lift(ExpOr(Seq(op, orElse)))
  }

}
