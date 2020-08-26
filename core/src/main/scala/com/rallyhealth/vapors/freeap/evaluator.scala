package com.rallyhealth.vapors.freeap

import cats.arrow.FunctionK
import cats.data.{NonEmptyList, State}
import com.rallyhealth.vapors.{Fact, FactsMatch, NoFactsMatch, Result}

object evaluator {

  import algebra._
  import dsl._

  def evaluate[X](
    allFacts: List[Fact[X]],
    root: ExpDsl[X]
  ): Result[X] = {
    type Evaluator[A] = State[Result[X], A]

    def eval[Y](exp: ExpDsl[Y]): Evaluator[Y] = {
      exp
        .foldMap(new FunctionK[ExpAlg, Evaluator] {
          override def apply[A](fa: ExpAlg[A]): Evaluator[A] = {
            val a = for {
              c <- State.get[Result[X]]
              _ <- State.set(fa match {
                case ExpHas(v) =>
                  Result.fromList(c.matchingFacts.filter(_.value == v))
                case ExpAnd(e) =>
                  e.foldLeft(c) {
                    case (f @ FactsMatch(matching), n) =>
                      // TODO: Is this correct?
                      eval(n).runS(f).value
                  }
              })
              x <- State.get[Result[X]]
            } yield x
            a
          }
        })
    }

    NonEmptyList.fromList(allFacts).map { allFactsNel =>
      eval(root)
        .runS(FactsMatch(allFactsNel))
        .value
    }.getOrElse(NoFactsMatch)
  }

}
