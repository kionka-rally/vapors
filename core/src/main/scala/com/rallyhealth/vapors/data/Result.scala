package com.rallyhealth.vapors.data

import cats.data.NonEmptyList

sealed trait Result[+I] {

  def matchingFacts: List[Fact[I]]

  @inline final def isEmpty: Boolean = matchingFacts.isEmpty
  @inline final def nonEmpty: Boolean = matchingFacts.nonEmpty

  // TODO: Is this needed? Will this make things more complicated later when we need to propagate information?
  final def filter(p: Fact[I] => Boolean): Result[I] = {
    this match {
      case NoFactsMatch =>
        NoFactsMatch
      case FactsMatch(matchingFacts) =>
        Result.fromList(matchingFacts.filter(p))
    }
  }
}

object Result {

  def fromNel[I](nonEmptyFacts: NonEmptyList[Fact[I]]): FactsMatch[I] = FactsMatch(nonEmptyFacts)

  def fromList[I](facts: List[Fact[I]]): Result[I] = NonEmptyList.fromList(facts) match {
    case Some(nonEmptyList) => FactsMatch(nonEmptyList)
    case None => NoFactsMatch
  }
}

final case class FactsMatch[+I](matchingFactsNel: NonEmptyList[Fact[I]]) extends Result[I] {
  override def matchingFacts: List[Fact[I]] = matchingFactsNel.toList
}

case object NoFactsMatch extends Result[Nothing] {
  override def matchingFacts: List[Fact[Nothing]] = Nil
}
