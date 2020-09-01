package com.rallyhealth.vapors.adt

import cats.data.NonEmptyList
import com.rallyhealth.vapors.data._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object evaluator {

  import algebra._
  import com.rallyhealth.collections.ops._

  def evaluate[A, B : ClassTag : TypeTag](
    allFacts: List[Fact[A]],
    root: AlgExp[B]
  ): Result[B] = {

    def eval[Y](
      facts: NonEmptyList[Fact[Y]],
      exp: AlgExp[Y]
    ): Result[Y] = {
      exp match {

        // TODO: Put a better error message here
        case FailedExp() => NoFactsMatch()

        case HasAnyExp(dataset) =>
          val valueSet = dataset.collect {
            case Value(v) => v
          }
          Result.fromList(facts.filter { f =>
            dataset.contains(f) || valueSet.contains(f.value)
          })

        case e: WithFactTypesExp[_, Y] =>
          NonEmptyList
            .fromList(facts.collect {
              case e.factTypes.Match(f) => f
            })
            .map { subFacts =>
              val res = eval(subFacts, e.subExp)
              e.widen(res)
            }
            .getOrElse(NoFactsMatch())

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
            NoFactsMatch()
          } else {
            Result.fromList(accumulatedMatchingFacts.toList)
          }

        case OrExp(expressions) =>
          expressions
            .foldWith[Result[Y]](NoFactsMatch())
            // stop after the first non-empty result
            .breakAfterState(_.nonEmpty) {
              case (_, nextExp) =>
                eval(facts, nextExp)
            }
      }
    }

    val validResult = for {
      allTypeSet <- FactTypeSet.fromList(allFacts.map(_.typeInfo))
      validTypeSet = allTypeSet.subset[B].getOrElse(FactTypeSet.empty[B])
      typedExp = WithFactTypesExp[B, B](validTypeSet, root)
      validFacts <- NonEmptyList.fromList(allFacts.collect {
        case validTypeSet.Match(f) => f
      })
    } yield {
      eval(validFacts, typedExp)
    }
    validResult.getOrElse(NoFactsMatch())
  }

}
