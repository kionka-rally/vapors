package com.rallyhealth.vapors.freeap

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

  s"$it should combine matching facts from and operator" in {
    val result = evaluate(
      Facts.all,
      has(Facts.age) && has(Facts.probs)
    )
    assertResult(List(Facts.age, Facts.probs)) {
      result.matchingFacts
    }
  }
}

case class Probs(scores: Map[String, Double])
