package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.{Data, Fact, Value}

import scala.reflect.ClassTag

object dsl {
  import algebra._

  def has[A](exp: Data[A]): ExpAlg[A] = ExpHas(exp)

  def hasValue[A](exp: A): ExpAlg[A] = ExpHas(Value(exp))

  def hasAny[A](values: Set[Data[A]]): ExpAlg[A] = ExpHasAny(values)

  def withType[B]: WithType[B] = new WithType[B]

  final class WithType[B] private[dsl] {
    def apply[A >: B](exp: ExpAlg[B])(implicit ct: ClassTag[B]): ExpAlg[A] = ExpTyped[A, B](exp)
  }

  def and[A](expressions: ExpAlg[A]*): ExpAlg[A] = ExpAnd(expressions.toList)

  def or[A](expressions: ExpAlg[A]*): ExpAlg[A] = ExpOr(expressions.toList)

  def filter[A](predicate: Fact[A] => Boolean): ExpAlg[A] = ExpFilter(predicate)

  //  implicit class ExpOps[A](val op: ExpAlg[A]) extends AnyVal {
  //
  //    def &&[B >: A](andThen: ExpAlg[B]): ExpAlg[B] = ExpAnd(List(op, andThen))
  //
  //    def ||(orElse: ExpAlg[A]): ExpAlg[A] = ExpOr(List(op, orElse))
  //  }

}
