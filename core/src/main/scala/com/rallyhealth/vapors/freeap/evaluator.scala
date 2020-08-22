package com.rallyhealth.vapors.freeap

import cats.{Applicative, Monad}
import cats.data.{ContT, Kleisli, NonEmptyList, OneAnd, State, StateT}
import cats.free.FreeApplicative
import com.rallyhealth.vapors.{Fact, Result}

object evaluator {

  import algebra._
  import cats.implicits._
  import cats.syntax.all._
  import dsl._

//  type FactFilter[I, O] = Seq[Fact[I]] => Seq[Data[O]]
//  private class ApplicativeEvaluator[I] extends Applicative[({ type F[O] = FactFilter[I, O]})#F] {
//    override def pure[A](x: A): FactFilter[I, A] = _ => Seq(Value(x))
//    override def ap[A, B](ff: FactFilter[I, A => B])(fa: FactFilter[I, A]): FactFilter[I, B] = {
//      ff.andThen(fns => fns.map(fn => fn.value.andThen()))
//    }
//  }

//  type Op[X, A] = (Seq[Fact[X]], Seq[Fact[X]] => A) => A

//  private class ApplicativeOp[X] extends Applicative[({ type F[A] = Op[X, A]})#F] {
//    override def pure[A](x: A): Op[X, A] = (_, _) => x
//    override def ap[A, B](ff: Op[X, A => B])(fa: Op[X, A]): Op[X, B] = { (facts, op) =>
//      val z = ff(facts, fs => {
//        val b = op(fs)
//        val y = fa(fs, op.compose())
//        y
//        b
//      })
//    }
//  }

//  private class ApplicativeEvaluator[I] extends Applicative[Op] {
//    override def pure[A](x: A): Op[A] = Seq(x)
//    override def ap[A, B](ff: Op[A => B])(fa: Op[A]): Op[B] = { facts =>
//      ff(facts) match {
//        case NoFactsMatch => NoFactsMatch
//        case FactsMatch(filtered, next) =>
//          fa(filtered) match {
//            case NoFactsMatch => NoFactsMatch
//            case FactsMatch(remaining, a) =>
//              FactsMatch(remaining, next(a))
//          }
//      }
//    }
//  }

  def eval[X](
    allFacts: Seq[Fact[X]],
    root: ExpDsl[X]
  ): Result[X] = {

//    def eval0[A](currentResult: Result[X], exp: ExpAlg[A]): Result[X] = {
//      exp match {
//        case ExpHas(v) =>
//          Result.fromList(currentResult.matchingFacts.filter(_.value == v))
//        case ExpAnd(e) =>
//          e.foldLeft(currentResult) {
//            case (f @ FactsMatch(matching), n) =>
//              eval0(f, n.map(a => a))
//              matching
//          }
//      }
//    }
    ???

  }

  def evaluate[X](
    allFacts: Seq[Fact[X]],
    root: ExpDsl[X]
  ): List[Fact[X]] = {
    type Evaluator[A] = StateT[Result, X, A]

//    val filter = root
//      .foldMap(new FunctionK[ExpAlg, Evaluator] {
//        override def apply[A](fa: ExpAlg[A]): Evaluator[A] = {
//          for {
//            c <- State.get[Result[X]]
//            x <- State.pure(fa match {
//              case ExpHas(v) =>
//                c.matchingFacts.exists(_.value == v)
//              case ExpAnd(e) =>
//                e.foldLeft(c) {
//                  case (FactsMatch(matching), n) =>
//                    evaluate()
//                    matching
//                }
//            })
//          } yield {
//            c
//          }
//        }
//      })
//    filter(allFacts.toList)
//      .runS(allFacts)
//      .value
    ???
  }

}
