package com.rallyhealth.vapors.tagless.algebra

import cats.Applicative
import com.rallyhealth.vapors.data.{Data, Result}

import scala.reflect.runtime.{universe => ru}

trait ExpAlg[F[_]] extends Applicative[F] {

  // TODO: How do I implement this?
//  def and[I](expressions: List[ExpDsl[I]]): F[Result[I]]

  def hasAnyOf[I](v: Set[Data[I]]): F[Result[I]]

  def hasTypeOf[I](implicit tt: ru.TypeTag[Data[I]]): F[Result[I]]
}

object ExpAlg {

  def apply[F[_]](implicit alg: ExpAlg[F]): ExpAlg[F] = alg
}
