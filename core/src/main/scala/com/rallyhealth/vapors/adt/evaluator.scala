package com.rallyhealth.vapors.adt

import cats.data.NonEmptyList
import com.rallyhealth.vapors.data.{Fact, FactsMatch, NoFactsMatch, Result}

object evaluator {

  import algebra._
  import com.rallyhealth.collections.ops._

  def evaluate[A](allFacts: List[Fact[A]], root: AlgExp[A]): Result[A] = {

    def eval[Y](
      facts: NonEmptyList[Fact[Y]],
      exp: AlgExp[Y]
    ): Result[Y] = {
      exp match {

        case WithNameExp(names, subExp) =>
          NonEmptyList
            .fromList(facts.filter(names contains _.typeInfo.name))
            .map { subFacts =>
              eval(subFacts, subExp)
            }
            .getOrElse(NoFactsMatch)

        case x: WithTypeExp[Y, _] =>
          NonEmptyList
            .fromList(facts.collect(Function.unlift(x.cast)))
            .map { subFacts =>
              eval[x.Sub](subFacts, x.subExp)
            }
            .getOrElse(NoFactsMatch)

        case HasExp(datum) =>
          Result.fromList(facts.filter(_ == datum))

        case HasAnyExp(dataset) =>
          Result.fromList(facts.find(dataset.contains).toList)

        case WithinWindowExp(range) =>
          Result.fromList(facts.filter(f => range.contains(f.value)))

        case AndExp(expressions) =>
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

        case OrExp(expressions) =>
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
