package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyList
import com.rallyhealth.vapors.data.{Fact, FactsMatch, NoFactsMatch, Result}

object evaluator {

  import algebra._

  def evaluate[X](
    allFacts: List[Fact[X]]
  )(
    root: ExpAlg[X]
  ): Result[X] = {

    def eval[Y](
      facts: NonEmptyList[Fact[Y]],
      exp: ExpAlg[Y]
    ): Result[Y] = {
      exp match {
        case x: ExpTyped[Y, _] =>
          NonEmptyList.fromList(x.filter(facts)).map { subFacts =>
            eval(subFacts, x.subExp)
          }.getOrElse(NoFactsMatch)
        case ExpHas(datum) => Result.fromList(facts.filter(_ == datum))
        case ExpHasAny(dataset) =>
          Result.fromList(facts.find(dataset).toList)
        case ExpFilter(fn) =>
          Result.fromList(facts.filter(fn))
        case ExpAnd(expressions) =>
          var currentResult: Result[Y] = FactsMatch(facts)
          var accumulatedMatchingFacts: Set[Fact[Y]] = Set()
          val iter = expressions.iterator
          while (iter.hasNext && currentResult.matchingFacts.nonEmpty) {
            val nextExp = iter.next()
            val filtered = eval(facts, nextExp)
            accumulatedMatchingFacts ++= filtered.matchingFacts
            currentResult = filtered
          }
          if (currentResult.matchingFacts.isEmpty) {
            NoFactsMatch
          } else {
            Result.fromList(accumulatedMatchingFacts.toList)
          }
        case ExpOr(expressions) =>
          var currentResult: Result[Y] = NoFactsMatch
          val iter = expressions.iterator
          while (iter.hasNext && currentResult.matchingFacts.isEmpty) {
            val nextExp = iter.next()
            val filtered = eval(facts, nextExp)
            currentResult = filtered
          }
          currentResult
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
