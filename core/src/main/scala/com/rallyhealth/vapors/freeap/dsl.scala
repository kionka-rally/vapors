package com.rallyhealth.vapors.freeap

import cats.free.FreeApplicative
import cats.implicits._
import com.rallyhealth.vapors.Data

// TODO: Is FreeApplicative worth it?
object dsl {
  import algebra._

  type ExpDsl[A] = FreeApplicative[ExpAlg, A]

  private def lift[A](value: ExpAlg[A]): ExpDsl[A] = FreeApplicative.lift(value)

  def has[A](exp: Data[A]): ExpDsl[A] = lift(ExpHas(exp))

  def and[A](expressions: ExpDsl[A]*): ExpDsl[A] = expressions.reduce(_ *> _)

  def or[A](expressions: ExpDsl[A]*): ExpDsl[A] = expressions.reduce(_ *> _)

  implicit class DslOps[A](val op: ExpDsl[A]) extends AnyVal {

    def &&(andThen: ExpDsl[A]): ExpDsl[A] = and(op, andThen)

    def ||(orElse: ExpDsl[A]): ExpDsl[A] = or(op, orElse)
  }

}
