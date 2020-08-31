package com.rallyhealth.vapors.data

import scala.collection.immutable.NumericRange

sealed trait Window[A] {

  def contains(value: A): Boolean
}

object Window {
  import Ordering.Implicits._

  def fromRange(range: Range): Window[Int] = {
    Window.between(range.start, includeStart = true, range.end, includeEnd = range.isInclusive)
  }

  def fromRange[A : Ordering](range: NumericRange[A]): Window[A] = {
    Window.between(range.start, includeStart = true, range.end, includeEnd = range.isInclusive)
  }

  def before[A : Ordering](end: A, inclusive: Boolean): Window[A] = Before(end, inclusive)

  def before[A : Ordering](end: A): Window[A] = Before(end, inclusive = false)

  def onOrBefore[A : Ordering](end: A): Window[A] = Before(end, inclusive = true)

  def after[A : Ordering](start: A, inclusive: Boolean): Window[A] = After(start, inclusive)

  def after[A : Ordering](start: A): Window[A] = After(start, inclusive = false)

  def onOrAfter[A : Ordering](start: A): Window[A] = After(start, inclusive = true)

  def between[A : Ordering](start: A, includeStart: Boolean, end: A, includeEnd: Boolean): Window[A] =
    Between(start, includeStart, end, includeEnd)

  def between[A : Ordering](start: A, end: A): Window[A] =
    Between(start, includeStart = true, end, includeEnd = false)

  def betweenExclusive[A : Ordering](start: A, end: A): Window[A] =
    Between(start, includeStart = false, end, includeEnd = false)

  def betweenInclusive[A : Ordering](start: A, end: A): Window[A] =
    Between(start, includeStart = true, end, includeEnd = true)

  def afterUntil[A : Ordering](start: A, end: A): Window[A] =
    Between(start, includeStart = false, end, includeEnd = true)

  final case class Before[A : Ordering](end: A, inclusive: Boolean) extends Window[A] {

    private val checkBeforeEnd: A => Boolean = {
      if (inclusive) _ <= end
      else _ < end
    }

    override def contains(value: A): Boolean = checkBeforeEnd(value)
  }

  final case class After[A : Ordering](start: A, inclusive: Boolean) extends Window[A] {

    private val checkAfterStart: A => Boolean = {
      if (inclusive) _ <= start
      else _ < start
    }

    override def contains(value: A): Boolean = checkAfterStart(value)
  }

  final case class Between[A : Ordering](start: A, includeStart: Boolean, end: A, includeEnd: Boolean)
    extends Window[A] {

    private val checkBetween: A => Boolean = (includeStart, includeEnd) match {
      case (true, true)   => a => a >= start && a <= end
      case (true, false)  => a => a >= start && a <  end
      case (false, true)  => a => a >  start && a <= end
      case (false, false) => a => a >  start && a <  end
    }

    override def contains(value: A): Boolean = checkBetween(value)
  }
}
