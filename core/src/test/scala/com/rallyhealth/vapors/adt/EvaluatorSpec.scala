package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.adt.algebra.AlgExp
import com.rallyhealth.vapors.data.Fact
import org.scalatest.freespec.AnyFreeSpec

class EvaluatorSpec extends AnyFreeSpec {
  import dsl._
  import evaluator._

  private final val it = evaluator.getClass.getName.dropRight(1).split('.').takeRight(2).mkString(".")

  private final case class Probs(scores: Map[String, Double])

  private final object Facts {
    val name = Fact("name", "Joe Schmoe")
    val age = Fact("age", 32)
    val weight = Fact("weight", 150)
    val probs = Fact("probs", Probs(Map("weightloss" -> .8)))

    val all = List(
      name,
      age,
      weight,
      probs
    )
  }

  s"$it should filter facts with a certain name" in {
    val exp: AlgExp[Any] = {
      withType[Int] {
        withFactsNamed(Facts.age.name) {
          withinRange(20 to 40)
        }
      }
    }
    val result = evaluate(Facts.all)(exp)
    assertResult(List(Facts.age)) {
      result.matchingFacts
    }
  }

  s"$it should combine matching facts using the && operator" in {
    val exp: AlgExp[Any] = {
      and(
        withType[Int](withinRange(20 to 30)),
        has(Facts.probs)
      )
    }
    val result = evaluate(Facts.all)(exp)
    assertResult(List(Facts.age, Facts.probs)) {
      result.matchingFacts
    }
  }

  s"$it should filter to the first matching set of facts using the || operator" in {
    val exp: AlgExp[Any] = {
      or(
        withType[Int] {
          withinRange(70 to 100)
        },
        has(Facts.weight)
      )
    }
    val result = evaluate(Facts.all)(exp)
    assertResult(List(Facts.weight)) {
      result.matchingFacts
    }
  }

  s"$it should filter to only facts with the specified type of value" in {
    val exp: AlgExp[Any] = withType[Probs] {
      hasValue(Facts.probs.value)
    }
    val result = evaluate(Facts.all)(exp)
    assertResult(List(Facts.probs)) {
      result.matchingFacts
    }
  }
}
