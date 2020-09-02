package com.rallyhealth.vapors.freeap

import cats.data.NonEmptyVector
import com.rallyhealth.vapors.Fact
import org.scalatest.freespec.AnyFreeSpec

class EvaluatorSpec extends AnyFreeSpec {
  import evaluator._
  import dsl._

  private val it = "freeap.evaluate"

  private object Facts {
    val name = Fact("name", "Joe Schmoe")
    val age = Fact("age", 32)
    val probs = Fact("probs", Probs(Map("weightloss" -> .8)))

    val all = List(
      Facts.name,
      Facts.age,
      Facts.probs
    )
  }

  def v[A](head: A, tail: A*): NonEmptyVector[A] = NonEmptyVector.of(head, tail: _*)
  val True: Any => Boolean = _ => true
  val False: Any => Boolean = _ => false

  s"$it should return true for one true in an or" in {
    val program: ExpDsl[Boolean] = or(v(has(True)))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return true for one true and one false in an or" in {
    val program = or(v(has(True), has(False)))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return true for a complex or" in {
    val program = or(v(has(False), has(False), has(True), has(False)))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return false for a long or" in {
    val program = or(v(has(False), has(False), has(False), has(False)))
    val result = eval(program)(new BigData)
    assert(!result)
  }

  s"$it should return true for one true in an and" in {
    val program = and(v(has(True)))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return false for a true and false in an and" in {
    val program = and(v(has(True), has(False)))
    val result = eval(program)(new BigData)
    assert(!result)
  }

  s"$it should return true for two trues in an and" in {
    val program = and(v(has(True), has(True)))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return false for a complex and" in {
    val program = and(v(has(True), has(True), has(False), has(True)))
    val result = eval(program)(new BigData)
    assert(!result)
  }

  s"$it should return true for a long and" in {
    val program = and(v(has(True), has(True), has(True), has(True)))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return true for nested true 'or's in an and" in {
    val program =
      and(v(
        or(v(has(True))),
        or(v(has(True))),
      ))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return false for a nested false or in an and" in {
    val program =
      and(v(
        or(v(has(True))),
        or(v(has(False))),
      ))
    val result = eval(program)(new BigData)
    assert(!result)
  }

  s"$it should return false for a nested false or in a long and" in {
    val program =
      and(v(
        or(v(has(True))),
        or(v(has(True))),
        or(v(has(False))),
        or(v(has(True))),
      ))
    val result = eval(program)(new BigData)
    assert(!result)
  }

  s"$it should return true for a nested false and in an or" in {
    val program =
      or(v(
        and(v(has(False))),
        and(v(has(False))),
        and(v(has(True))),
        and(v(has(False))),
      ))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should return true for a complex structure" in {
    val program =
      or(v(
        and(v(
          has(False),
          has(True))
        ),
        and(v(has(False))),
        and(v(has(False))),
        or(v(
          and(v(
            has(True),
            has(False)
          )),
          has(True)
        ))
      ))
    val result = eval(program)(new BigData)
    assert(result)
  }

  s"$it should combine matching facts from and operator" in {
    // val result = evaluate(
    //   Facts.all,
    //   has(Facts.age) && has(Facts.probs)
    // )
    // assertResult(List(Facts.age, Facts.probs)) {
    //   result.matchingFacts
    // }
  }
}

case class Probs(scores: Map[String, Double])
