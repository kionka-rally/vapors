package com.rallyhealth.vapors.adt

import com.rallyhealth.vapors.adt.algebra.AlgExp
import com.rallyhealth.vapors.data.{Fact, FactType}
import org.scalatest.freespec.AnyFreeSpec

class EvaluatorSpec extends AnyFreeSpec {
  import dsl._
  import evaluator._

  private final val it = evaluator.getClass.getName.dropRight(1).split('.').takeRight(2).mkString(".")

  private final case class Probs(scores: Map[String, Double])

  private final object FactTypes {
    val name = FactType[String]("name")
    val age = FactType[Int]("age")
    val weight = FactType[Int]("weight")
    val probs = FactType[Probs]("probability_to_use")
  }

  private final object Facts {
    val name = Fact(FactTypes.name, "Joe Schmoe")
    val age = Fact(FactTypes.age, 32)
    val weight = Fact(FactTypes.weight, 150)
    val probs = Fact(FactTypes.probs, Probs(Map("weightloss" -> .8)))

    val all = List(
      name,
      age,
      weight,
      probs
    )
  }

  s"$it should filter facts with a certain name" in {
    val exp = {
      withFactType(FactTypes.age) {
        withinRange(20 to 40)
      }
    }
    val result = evaluate(Facts.all, exp)
    assertResult(List(Facts.age)) {
      result.matchingFacts
    }
  }

  s"$it should combine matching facts using the && operator" in {
    val exp = {
      and(
        withFactType(FactTypes.age) {
          withinRange(20 to 30)
        },
        has(Facts.probs)
      )
    }
    val result = evaluate(Facts.all, exp)
    assertResult(List(Facts.age, Facts.probs)) {
      result.matchingFacts
    }
  }

  s"$it should filter to the first matching set of facts using the || operator" in {
    val exp = {
      or(
        withFactType(FactTypes.age) {
          withinRange(70 to 100)
        },
        has(Facts.probs)
      )
    }
    val result = evaluate(Facts.all, exp)
    assertResult(List(Facts.weight)) {
      result.matchingFacts
    }
  }

  s"$it should filter to only facts with the specified type of value" in {
//    val exp: AlgExp[Any] = withFactType(FactTypes.probs) {
//      has(Facts.probs)
//    }
    val exp = withFactType(FactTypes.probs) {
      has(Facts.probs)
    }
    val result = evaluate(Facts.all, exp)
    assertResult(List(Facts.probs)) {
      result.matchingFacts
    }
  }
}
