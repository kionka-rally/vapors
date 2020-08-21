package com.rallyhealth.examples.confree

object dsl {
  import algebra._
  import cats.free.FreeApplicative

  type Dsl[A] = FreeApplicative[ConfigF, A]

  private def lift[A](value: ConfigF[A]): Dsl[A] = FreeApplicative.lift[ConfigF, A](value)

  def int(field: String): Dsl[Int] = lift(ConfigInt(field, identity))
  def flag(field: String): Dsl[Boolean] = lift(ConfigFlag(field, identity))
  def port(field: String): Dsl[Int] = lift(ConfigPort(field, identity))
  def server(field: String): Dsl[String] = lift(ConfigServer(field, identity))
  def file(field: String): Dsl[String] = lift(ConfigFile(field, identity))
  def sub[A](field: String)(value: Dsl[A]): Dsl[A] = lift(ConfigSub(field, value))
}
