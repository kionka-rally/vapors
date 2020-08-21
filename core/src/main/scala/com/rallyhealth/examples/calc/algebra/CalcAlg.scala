package com.rallyhealth.examples.calc.algebra

import cats.Applicative

import scala.language.higherKinds

trait CalcAlg[F[_]] extends Applicative[F] {

  def push(v: Int): F[Unit]

  def pop: F[Int]

  def sum: F[Int]

  def mul: F[Int]
}

object CalcAlg {

  def apply[F[_]](implicit asm: CalcAlg[F]): CalcAlg[F] = asm
}
