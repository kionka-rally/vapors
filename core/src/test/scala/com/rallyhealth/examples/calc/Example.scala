package com.rallyhealth.examples.calc

object Example {
  import cats.syntax.apply._
  import com.rallyhealth.examples.calc.dsl._

  def calc(): Unit = {
    val program =
      push(1) *>
        push(2) *>
        sum *>
        push(9) *>
        mul

    val printed = printer.print(program)
    val evaled = evaluator.evaluate(program)
    println("PRINTED: " + printed)
    println("evaled: " + evaled)
  }

  def main(args: Array[String]): Unit = {
    calc()
  }
}
