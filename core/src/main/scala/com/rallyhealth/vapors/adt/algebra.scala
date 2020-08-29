package com.rallyhealth.vapors.adt

import cats.data.NonEmptyList
import com.rallyhealth.vapors.{Data, Fact}

import scala.reflect.ClassTag

object algebra {

  sealed trait ExpAlg[-A]

  case class ExpHas[A](value: Data[A]) extends ExpAlg[A]

  // TODO: Should I use a Seq[Data[A]] to be covariant
  case class ExpHasAny[A](dataset: Set[Data[A]]) extends ExpAlg[A]

  //  case class ExpFn[-A, +B](
  //    data: NonEmptyList[Data[A]] => ExpAlg[B],
  //    convert: A => B
  //  ) extends ExpAlg[B]

  //  case class ExpMap[+A](convert: A) extends ExpAlg[A] {
  //    def map
  //  }

  case class ExpTyped[A, B <: A](exp: ExpAlg[B])(implicit val classTag: ClassTag[B]) extends ExpAlg[A] {
    type Sub = B

    def filter(facts: NonEmptyList[Fact[A]]): List[Fact[Sub]] = {
      facts.collect {
        case f: Fact[B] if classTag.unapply(f.value).isDefined => f
      }
    }

    def subExp: ExpAlg[Sub] = exp
  }

  // TODO: This is too powerful and unserializable. We should use an ExpAlg instead of a function
  case class ExpFilter[-A](predicate: Fact[A] => Boolean) extends ExpAlg[A]

  case class ExpAnd[-A](expressions: List[ExpAlg[A]]) extends ExpAlg[A]

  case class ExpOr[-A](expressions: List[ExpAlg[A]]) extends ExpAlg[A]
}
