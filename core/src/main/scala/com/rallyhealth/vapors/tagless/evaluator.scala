package com.rallyhealth.vapors.tagless

import cats.{Applicative, ~>}
import cats.arrow.FunctionK
import cats.data.Const
import cats.free.FreeApplicative
import com.rallyhealth.vapors.{Data, Result}
import com.rallyhealth.vapors.tagless.algebra.ExpAlg

import scala.reflect.runtime.{universe => ru}

object evaluator {

  import cats.data.State
  import cats.syntax.apply._
  import dsl._

  private type EvaluatorState[X] = Result[X]

  private type Evaluator[X, A] = State[Result[X], A]

  private class EvaluatorF[X] extends ExpAlg[({ type F[A] = Evaluator[X, A] })#F] {

    override def pure[A](x: A): Evaluator[X, A] = State.pure(x)

    override def ap[A, B](ff: Evaluator[X, A => B])(fa: Evaluator[X, A]): Evaluator[X, B] = ff <*> fa

//    override def and[I](expressions: Seq[FreeApplicative[ExpAlg, I]]): Evaluator[X, Result[I]] = {
//      type G[A] = Const[List[Fact[X]], A]
//      for {
//        c <- State.get[EvaluatorState[X]]
//      } yield {
//        val x = expressions.foldLeft(c) {
//          case (s, n) =>
//            n.foldMap(new (ExpAlg ~> G) {
//              override def apply[A](fa: ExpAlg[A]): G[A] = {
//                fa
//              }
//            })
//            // TODO: Get the facts that match from applying each expression and if they are ever empty, then bail
//            s.filter()
//        }
//        x
//      }
//      ???
//    }

    override def hasAnyOf[I](v: Set[Data[I]]): Evaluator[X, Result[I]] = ???

    override def hasTypeOf[I](implicit tt: ru.TypeTag[Data[I]]): Evaluator[X, Result[I]] = ???
  }

//  def evaluate[X](exp: ExpDsl[X]): X = {
//    exp(Evaluator).runA(Nil).value
//  }
}
