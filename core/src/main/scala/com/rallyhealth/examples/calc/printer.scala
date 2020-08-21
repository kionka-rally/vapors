package com.rallyhealth.examples.calc

object printer {
  import com.rallyhealth.examples.calc.algebra._
  import dsl._

  private type Printer[A] = List[String]

  private object Printer extends CalcAlg[Printer] {

    override def pure[A](x: A): Printer[A] = Nil

    override def ap[A, B](ff: Printer[A => B])(fa: Printer[A]): Printer[B] = ff ::: fa

    override def push(v: Int): List[String] = "push " :: String.valueOf(v) :: "\n" :: Nil

    override def pop: List[String] = "pop\n" :: Nil

    override def sum: List[String] = "sum\n" :: Nil

    override def mul: List[String] = "mul\n" :: Nil
  }

  def print[X](exp: Dsl[X]): String = {
    val chunks = exp(Printer)
    val sb = new StringBuilder
    for (c <- chunks) {
      sb.append(c)
    }
    sb.result()
  }

}
