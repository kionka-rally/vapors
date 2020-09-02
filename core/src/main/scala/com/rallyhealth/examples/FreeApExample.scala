package com.rallyhealth.examples

import cats.arrow.FunctionK
import cats.free.FreeApplicative
import cats.implicits._

object FreeApExample {
  sealed abstract class ValidationOp[A]
  case class Size(size: Int) extends ValidationOp[Boolean]
  case object HasNumber extends ValidationOp[Boolean]
  // case object HasString extends ValidationOp[String]

  type Validation[A] = FreeApplicative[ValidationOp, A]

  def size(size: Int): Validation[Boolean] = FreeApplicative.lift(Size(size))

  val hasNumber: Validation[Boolean] = FreeApplicative.lift(HasNumber)

  type FromString[A] = String => A

  val compiler = new FunctionK[ValidationOp, FromString] {
    def apply[A](fa: ValidationOp[A]): FromString[A] = str =>
      fa match {
        case Size(size) => (str.size >= size)
        case HasNumber  => str.exists(c => "0123456789".contains(c))
        // case HasString => str
      }
  }

  val prog: Validation[Boolean] = (size(5), hasNumber).mapN { case (l, r) => l && r}

  val validator = prog.foldMap[FromString](compiler)

  validator("1234")
}
