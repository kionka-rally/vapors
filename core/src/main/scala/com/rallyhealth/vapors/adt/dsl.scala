package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.data.{Data, Value, Window}

import scala.collection.immutable.NumericRange
import scala.reflect.ClassTag

object dsl {
  import algebra._

  def has[A](data: Data[A]): AlgExp[A] = HasAnyExp(Set(data))

  def hasValue[A](value: A): AlgExp[A] = HasAnyExp(Set(Value(value)))

  def hasAny[A](values: Set[Data[A]]): AlgExp[A] = HasAnyExp(values)

  def hasAnyValue[A](values: Set[A]): AlgExp[A] = HasAnyExp(values.map(Value(_)))

  def withinRange(range: Range): AlgExp[Int] = WithinWindowExp(Window.fromRange(range))

  def withinRange[A : Ordering](range: NumericRange[A]): AlgExp[A] = WithinWindowExp(Window.fromRange(range))

  def withinWindow[A](window: Window[A]): AlgExp[A] = WithinWindowExp(window)

  // TODO: Replace all of these with name / with type using FactType[V]
  def withFactsNamed[A](name: String)(exp: AlgExp[A]): AlgExp[A] = WithNameExp(Set(name), exp)

  def withFactsNamed[A](names: Set[String])(exp: AlgExp[A]): AlgExp[A] = WithNameExp(names, exp)

  def withType[B]: WithType[B] = new WithType[B]

  final class WithType[B] private[dsl] {
    def apply[A >: B](exp: AlgExp[B])(implicit ct: ClassTag[B]): AlgExp[A] = WithTypeExp(exp)
  }

  def and[A](expressions: AlgExp[A]*): AlgExp[A] = AndExp.fromVector(expressions.toVector)

  def or[A](expressions: AlgExp[A]*): AlgExp[A] = OrExp.fromVector(expressions.toVector)

  // TODO: Figure out how to do infix syntax without breaking type inference
//  implicit class ExpWidenSyntax[Alg[x], A](val op: Alg[A]) extends AnyVal {
//
//    def widen[B >: A](implicit canWiden: CanWiden[Alg, A, B]): Alg[B] = canWiden.widen(op)

    //    def &&[AlgB[x] <: ExpAlg[x], B >: A](andThen: AlgB[B])(implicit canWiden: CanWiden[Alg, A, B]): ExpAnd[B] = {
    //      op.widen[B] match {
    //        case self: ExpAnd[B] => ExpAnd(self.expressions :+ andThen)
    //        case self => ExpAnd(Vector[ExpAlg[B]](self, andThen))
    //      }
    //    }

    //    def ||[AlgB[x] <: ExpAlg[x], B <: A](orElse: AlgB[B])(implicit canWiden: CanWiden[AlgB, B, A]): ExpOr[B] = {
    //      op match {
    //        case self: ExpOr[A] => ExpOr.fromVector(self.expressions :+ orElse)
    //        case _ => ExpOr(op, orElse.widen[A])
    //      }
    //    }
//  }

//  implicit class ExpOps[A](val op: ExpAlg[A]) extends AnyVal {
//
//    def &&(andThen: ExpAlg[A]): ExpAnd[A] = op match {
//      case self: ExpAnd[A] => ExpAnd.fromVector(self.expressions :+ andThen)
//      case _ => ExpAnd(op, andThen)
//    }
//
//    def ||(orElse: ExpAlg[A]): ExpOr[A] = op match {
//      case self: ExpOr[A] => ExpOr.fromVector(self.expressions :+ orElse)
//      case _ => ExpOr(op, orElse)
//    }
//  }

//  trait CanWiden[Alg[_], A, B >: A] {
//    def widen(exp: Alg[A]): Alg[B]
//  }
//
//  object CanWiden {
//    def instance[Alg[_], A, B >: A](ev: Alg[A] => Alg[B]): CanWiden[Alg, A, B] = new CanWiden[Alg, A, B] {
//      override def widen(exp: Alg[A]): Alg[B] = ev(exp)
//    }
//
//    implicit def canWidenHas[A, B >: A]: CanWiden[ExpHas, A, B] = instance { exp =>
//      ExpHas(exp.value)
//    }
//  }

}
