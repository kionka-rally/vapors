package com.rallyhealth.vapors.tagless.algebra

import cats.Applicative
import cats.free.FreeApplicative
import com.rallyhealth.vapors.{Data, Result}

import scala.reflect.runtime.{universe => ru}

trait ExpAlg[F[_]] extends Applicative[F] {

  // TODO: How to do this?
//  def and[I](expressions: Seq[FreeApplicative[ExpAlg, I]]): F[Result[I]]

  def hasAnyOf[I](v: Set[Data[I]]): F[Result[I]]

  def hasTypeOf[I](implicit tt: ru.TypeTag[Data[I]]): F[Result[I]]
}

object ExpAlg {

  def apply[F[_]](implicit alg: ExpAlg[F]): ExpAlg[F] = alg
}
