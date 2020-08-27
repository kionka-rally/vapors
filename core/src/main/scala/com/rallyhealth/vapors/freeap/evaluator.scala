package com.rallyhealth.vapors.freeap

import cats.arrow.FunctionK
import cats.data.{NonEmptyList, State}
import com.rallyhealth.vapors.{Fact, FactsMatch, NoFactsMatch, Result, Value}

object evaluator {

  import algebra._

  def evaluate[X](
    allFacts: List[Fact[X]],
    root: ExpAlg[X]
  ): Result[X] = {

    def eval(
      facts: NonEmptyList[Fact[X]],
      exp: ExpAlg[X]
    ): Result[X] = {
      exp match {
        case ExpHas(datum) => Result.fromList(facts.filter(_ == datum))
        case ExpHasAny(data) =>
          val dataSet = data.toSet
          Result.fromList(facts.find(dataSet).toList)
        case ExpExists(fn) =>
          facts.filter(f => fn(f.value))
        case ExpAnd(expressions) =>
          var currentResult: Result[X] = FactsMatch(facts)
          var accumulatedMatchingFacts: Set[Fact[X]] = Set()
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
          var currentResult: Result[X] = NoFactsMatch
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
