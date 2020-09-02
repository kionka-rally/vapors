package com.rallyhealth.vapors.freeap

import cats.arrow.FunctionK
import cats.data.{NonEmptyList, State}
import com.rallyhealth.vapors.freeap.algebra.{ExpAlg, ExpAnd, ExpHas, ExpOr}
import com.rallyhealth.vapors.freeap.dsl.ExpDsl
import com.rallyhealth.vapors.{Fact, FactsMatch, NoFactsMatch, Result}
import cats.implicits._

object evaluator {

  // allFacts: List[Fact[X]],
  // def evaluate[X](
  //   bigData: BigData,
  //   root: ExpDsl[X]
  // ): Unit = {

    // type FromBigdata[A]

  type Evaluator[A] = BigData => A
  def eval[Y](exp: ExpDsl[Y]): Evaluator[Y] = {
      exp
        .foldMap(new FunctionK[ExpAlg, Evaluator] {
          override def apply[A](fa: ExpAlg[A]): Evaluator[A] = { bigData =>
            fa match {
              case ExpHas(predicate) => predicate(bigData)
              case ExpAnd(predicate, expressions) =>
                predicate(expressions.map(eval).map(_(bigData)))
              case ExpOr(predicate, expressions) =>
                predicate(expressions.map(eval).map(_(bigData)))
            }
      //      val a = for {
      //        c <- State.get[Result[X]]
      //        _ <- State.set(fa match {
      //          case ExpHas(v) =>
      //            Result.fromList(c.matchingFacts.filter(_.value == v))
      //          case ExpAnd(e) =>
      //            e.foldLeft(c) {
      //              case (f @ FactsMatch(matching), n) =>
      //                // TODO: Is this correct?
      //                eval(n).runS(f).value
      //            }
      //        })
      //        x <- State.get[Result[X]]
      //      } yield x
      //      a
          }
        })
    }

    // NonEmptyList.fromList(allFacts).map { allFactsNel =>
    //   eval(root)
    //     .runS(FactsMatch(allFactsNel))
    //     .value
    // }.getOrElse(NoFactsMatch)
}
