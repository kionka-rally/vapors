package com.rallyhealth.vapors

import cats.data.NonEmptyList

sealed trait Result[+I] {

  def matchingFacts: List[Fact[I]]

  final def filter(p: Fact[I] => Boolean): Result[I] = {
    this match {
      case NoFactsMatch =>
        NoFactsMatch
      case FactsMatch(matchingFacts) =>
        Result.fromList(matchingFacts.filter(p))
    }
  }

//  final def withFacts[J](fn: NonEmptyList[Fact[I]] => List[Fact[J]]): Result[J] = this match {
//    case FactsMatch(matchingFacts) =>
//      fn(matchingFacts)
//    case NoFactsMatch =>
//      NoFactsMatch
//  }
//
//  final def flatMap[J](fn: List[Fact[I]] => Result[J]): Result[J] = this match {
//    case FactsMatch(matchingFacts) =>
//      NonEmptyList.fromList(matchingFacts.toList.flatMap { d =>
//        fn(d.value).matchingFacts
//      }).map(FactsMatch(_)).getOrElse(NoFactsMatch)
//    case NoFactsMatch =>
//      NoFactsMatch
//  }
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
