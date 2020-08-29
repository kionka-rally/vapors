package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.Fact
import org.scalatest.freespec.AnyFreeSpec

class EvaluatorSpec extends AnyFreeSpec {
  import dsl._
  import evaluator._

  private val it = "freeap.evaluate"

  private object Facts {
    val name = Fact("name", "Joe Schmoe")
    val age = Fact("age", 32)
    val probs = Fact("probs", Probs(Map("weightloss" -> .8)))

    val all = List(
      name,
      age,
      probs
    )
  }

  s"$it should combine matching facts using the && operator" in {
    val result = evaluate(Facts.all) {
      and(
        has(Facts.age),
        has(Facts.probs)
      )
    }
    assertResult(List(Facts.age, Facts.probs)) {
      result.matchingFacts
    }
  }

  s"$it should filter to the first matching set of facts using the || operator" in {
    val result = evaluate(Facts.all) {
      withType[Probs] {
        hasValue(32)
      }
    }
    assertResult(List(Facts.age)) {
      result.matchingFacts
    }
  }
}

case class Probs(scores: Map[String, Double])
