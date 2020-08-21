package com.rallyhealth.examples.calc

import scala.language.higherKinds

object dsl {
  import cats.Applicative
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import com.rallyhealth.examples.calc.algebra._

  sealed abstract class Dsl[A] {
    def apply[F[_] : CalcAlg]: F[A]
  }

  private final case class Pure[X](x: X) extends Dsl[X] {
    override def apply[F[_] : CalcAlg]: F[X] = x.pure[F]
  }

  private final case class ApplyDsl[A, B](
    ff: Dsl[A => B],
    fa: Dsl[A]
  ) extends Dsl[B] {
    override def apply[F[_] : CalcAlg]: F[B] = {
      // Alias for: Applicative[F].ap(ff.apply[F])(fa.apply[F])
      ff.apply[F] <*> fa.apply[F]
    }
  }

  private final case class Push(v: Int) extends Dsl[Unit] {
    override def apply[F[_] : CalcAlg]: F[Unit] = CalcAlg[F].push(v)
  }

  private final case object Pop extends Dsl[Int] {
    override def apply[F[_] : CalcAlg]: F[Int] = CalcAlg[F].pop
  }

  private final case object Sum extends Dsl[Int] {
    override def apply[F[_] : CalcAlg]: F[Int] = CalcAlg[F].sum
  }

  private final case object Mul extends Dsl[Int] {
    override def apply[F[_] : CalcAlg]: F[Int] = CalcAlg[F].mul
  }

  def push(v: Int): Dsl[Unit] = Push(v)

  def pop: Dsl[Int] = Pop

  def sum: Dsl[Int] = Sum

  def mul: Dsl[Int] = Mul

  implicit object ApplicativeDsl extends Applicative[Dsl] {

    override def pure[A](x: A): Dsl[A] = Pure(x)

    override def ap[A, B](ff: Dsl[A => B])(fa: Dsl[A]): Dsl[B] = ApplyDsl(ff, fa)
  }
}
