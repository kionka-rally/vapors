package com.rallyhealth.vapors.freeap

import com.rallyhealth.vapors.Fact
import org.scalatest.freespec.AnyFreeSpec

class EvaluatorSpec extends AnyFreeSpec {
  import evaluator._
  import algebra._

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
    val result = evaluate(
      Facts.all,
      ExpHas(Facts.age) && ExpHas(Facts.probs)
    )
    assertResult(List(Facts.age, Facts.probs)) {
      result.matchingFacts
    }
  }

  s"$it should filter to the first matching set of facts using the || operator" in {
    val result = evaluate(
      Facts.all,
      ExpExists(_.value)
    )
    assertResult(List(Facts.probs)) {
      result.matchingFacts
    }
  }
}

case class Probs(scores: Map[String, Double])
