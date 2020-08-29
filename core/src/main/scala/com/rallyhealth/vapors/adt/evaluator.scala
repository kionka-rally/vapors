package com.rallyhealth.vapors.adt

import cats.data.NonEmptyList
import com.rallyhealth.vapors.{Fact, FactsMatch, NoFactsMatch, Result}

object evaluator {

  import algebra._
  import com.rallyhealth.collections.ops._

  def evaluate[X](allFacts: List[Fact[X]])(root: ExpAlg[X]): Result[X] = {

    def eval[Y](
      facts: NonEmptyList[Fact[Y]],
      exp: ExpAlg[Y]
    ): Result[Y] = {
      exp match {
        case x: ExpTyped[Y, _] =>
          NonEmptyList
            .fromList(x.filter(facts))
            .map { subFacts =>
              eval(subFacts, x.subExp)
            }
            .getOrElse(NoFactsMatch)
        case ExpHas(datum) => Result.fromList(facts.filter(_ == datum))
        case ExpHasAny(dataset) =>
          Result.fromList(facts.find(dataset).toList)
        case ExpFilter(fn) =>
          Result.fromList(facts.filter(fn))
        case ExpAnd(expressions) =>
          type FoldState = (Result[Y], Set[Fact[Y]])
          val (result, accumulatedMatchingFacts) = expressions
            .foldWith[FoldState]((FactsMatch(facts), Set()))
            // break after the first empty result
            .breakAfterState {
              case (res, _) => res.isEmpty
            } {
              case ((_, acc), nextExp) =>
                val filtered = eval(facts, nextExp)
                (filtered, acc ++ filtered.matchingFacts)
            }
          if (result.matchingFacts.isEmpty) {
            NoFactsMatch
          } else {
            Result.fromList(accumulatedMatchingFacts.toList)
          }
        case ExpOr(expressions) =>
          expressions
            .foldWith[Result[Y]](NoFactsMatch)
            // stop after the first non-empty result
            .breakAfterState(_.nonEmpty) {
              case (_, nextExp) =>
                eval(facts, nextExp)
            }
      }
    }

    NonEmptyList
      .fromList(allFacts)
      .map { allFactsNel =>
        eval(allFactsNel, root)
      }
      .getOrElse(NoFactsMatch)
  }

}
