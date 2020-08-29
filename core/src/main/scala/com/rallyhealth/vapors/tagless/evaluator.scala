package com.rallyhealth.vapors.tagless

//import com.rallyhealth.vapors.tagless.algebra.ExpAlg
//import com.rallyhealth.vapors.{Data, Result}
//
//import scala.reflect.runtime.{universe => ru}

object evaluator {

//  import cats.data.State
//  import cats.syntax.apply._
//
//  private type Evaluator[X, A] = State[Result[X], A]
//
//  private class EvaluatorF[X] extends ExpAlg[({ type F[A] = Evaluator[X, A] })#F] {
//
//    type ThisState[A] = Evaluator[X, A]
//
//    override def pure[A](x: A): ThisState[A] = State.pure(x)
//
//    override def ap[A, B](ff: ThisState[A => B])(fa: ThisState[A]): ThisState[B] = ff <*> fa
//
//    override def and[I](expressions: List[ExpDsl[I]]): ThisState[Result[I]] = {
//      for {
//        initResult <- State.get[Result[I]]
//        combinedResult <- State.pure {
//          type FoldState = (Set[Fact[X]], Result[I])
//          val foldResult = expressions.foldM[Option, FoldState]((Set(), initResult)) {
//            case ((_, NoFactsMatch), _) => None
//            case ((acc, FactsMatch(facts)), node) =>
//              val exp = node.apply(this)
//              val result = exp.runS(initResult).value
//              Some((acc ++ facts.toList, result))
//          }
//          foldResult match {
//            case Some((usedFacts, finalResult)) =>
//              Result.fromList((usedFacts ++ finalResult.matchingFacts).toList)
//            case _ =>
//              NoFactsMatch
//          }
//        }
//        _ <- State.set(combinedResult)
//      } yield combinedResult
//    }
//
//    override def hasAnyOf[I](v: Set[Data[I]]): ThisState[Result[I]] = ???
//
//    override def hasTypeOf[I](implicit tt: ru.TypeTag[Data[I]]): ThisState[Result[I]] = ???
//  }
//
//  def evaluate[X](exp: ExpDsl[X]): X = {
//    exp(Evaluator).runA(Nil).value
//  }
}
