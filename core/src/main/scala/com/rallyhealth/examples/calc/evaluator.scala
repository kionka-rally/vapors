package com.rallyhealth.examples.calc

import scala.language.higherKinds

object evaluator {
  import cats.data.State
  import cats.syntax.apply._
  import com.rallyhealth.examples.calc.algebra._
  import dsl._

  private type EvaluatorState = List[Int]

  private type Evaluator[A] = State[List[Int], A]

  private object Evaluator extends CalcAlg[Evaluator] {

    override def pure[A](x: A): Evaluator[A] = State.pure(x)

    override def ap[A, B](ff: Evaluator[A => B])(fa: Evaluator[A]): Evaluator[B] = ff <*> fa

    def push(v: Int): Evaluator[Unit] = State.modify[EvaluatorState](v :: _)

    def pop: Evaluator[Int] = State.get[EvaluatorState].map(_.head)

    def sum: Evaluator[Int] =
      for {
        l <- State.get[EvaluatorState]
        (x1 :: x2 :: Nil, ys) = l.splitAt(2)
        v = x1 + x2
        _ <- State.set[EvaluatorState](v :: ys)
      } yield v

    def mul: Evaluator[Int] =
      for {
        l <- State.get[EvaluatorState]
        (x1 :: x2 :: Nil, ys) = l.splitAt(2)
        v = x1 * x2
        _ <- State.set[EvaluatorState](v :: ys)
      } yield v
  }

  def evaluate[X](exp: Dsl[X]): X = {
    exp(Evaluator).runA(Nil).value
  }
}
