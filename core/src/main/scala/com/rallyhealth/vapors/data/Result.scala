package com.rallyhealth.vapors.data

import cats.data.NonEmptyList

sealed trait Result[I] {

  def matchingFacts: List[Fact[I]]

  // TODO: Are the following methods be needed?
  // Or will they just make things more complicated later when needing to propagate info?

  @inline final def isEmpty: Boolean = matchingFacts.isEmpty

  @inline final def nonEmpty: Boolean = matchingFacts.nonEmpty

  final def collect[X](pf: PartialFunction[Fact[I], Fact[X]]): Result[X] = {
    Result.fromList(matchingFacts.collect(pf))
  }

  final def filter(p: Fact[I] => Boolean): Result[I] = {
    this match {
      case NoFactsMatch() =>
        NoFactsMatch()
      case FactsMatch(matchingFacts) =>
        Result.fromList(matchingFacts.filter(p))
    }
  }
}

object Result {

  def fromNel[I](nonEmptyFacts: NonEmptyList[Fact[I]]): FactsMatch[I] = FactsMatch(nonEmptyFacts)

  def fromList[I](facts: List[Fact[I]]): Result[I] = NonEmptyList.fromList(facts) match {
    case Some(nonEmptyList) => FactsMatch(nonEmptyList)
    case None => NoFactsMatch()
  }
}

final case class FactsMatch[I](matchingFactsNel: NonEmptyList[Fact[I]]) extends Result[I] {
  override def matchingFacts: List[Fact[I]] = matchingFactsNel.toList
}

final case class NoFactsMatch[I]() extends Result[I] {
  override def matchingFacts: List[Fact[I]] = Nil
}
