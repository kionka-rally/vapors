package com.rallyhealth.vapors.tagless

import cats.Applicative
import com.rallyhealth.vapors.{Result, Value}

import scala.reflect.runtime.{universe => ru}

object dsl {
  import algebra._
  import cats.syntax.applicative._
  import cats.syntax.apply._

  sealed abstract class ExpDsl[A] {
    def apply[F[_] : ExpAlg]: F[A]
  }

  final case class Pure[O](x: O) extends ExpDsl[O] {
    override def apply[F[_] : ExpAlg]: F[O] = x.pure[F]
  }

  private final case class ApplyNext[A, B](
    ff: ExpDsl[A => B],
    fa: ExpDsl[A]
  ) extends ExpDsl[B] {
    override def apply[F[_] : ExpAlg]: F[B] = {
      // Alias for: Applicative[F].ap(ff.apply[F])(fa.apply[F])
      ff.apply[F] <*> fa.apply[F]
    }
  }

  final case class HasAnyOf[I](values: Set[I]) extends ExpDsl[Result[I]] {
    override def apply[F[_] : ExpAlg]: F[Result[I]] = {
      ExpAlg[F].hasAnyOf[I](values.map(Value(_)))
    }
  }

  final case class HasType[I]()(implicit val tt: ru.TypeTag[I]) extends ExpDsl[Result[I]] {
    override def apply[F[_] : ExpAlg]: F[Result[I]] = {
      ExpAlg[F].hasTypeOf[I]
    }
  }

  final case class And[O](expressions: Seq[ExpDsl[O]]) extends ExpDsl[O] {
    override def apply[F[_] : ExpAlg]: F[O] = {
//      ExpAlg[F].and(expressions.map(d => d.apply[F]))
      ???
    }
  }

  def hasAnyOf[I, V](values: Set[V]): ExpDsl[Result[V]] = HasAnyOf(values)

  def has[V](value: V): ExpDsl[Result[V]] = HasAnyOf(Set(value))

  def and[V](exp: ExpDsl[V]*): ExpDsl[Result[V]] = {
//    ExpAnd(exp.map(_.apply[V]))
    ???
  }

  implicit object ApplicativeDsl extends Applicative[ExpDsl] {

    override def pure[A](x: A): ExpDsl[A] = Pure(x)

    override def ap[A, B](ff: ExpDsl[A => B])(fa: ExpDsl[A]): ExpDsl[B] = ApplyNext(ff, fa)
  }
}
