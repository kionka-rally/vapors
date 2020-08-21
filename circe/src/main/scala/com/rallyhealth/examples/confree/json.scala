package com.rallyhealth.examples.confree

object json {
  import algebra._
  import cats.arrow.FunctionK
  import cats.instances.either._
  import cats.syntax.functor._
  import dsl.Dsl
  import io.circe._

  def genDecode[X](config: Dsl[X]): Decoder[X] = {
    config.foldMap(new FunctionK[ConfigF, Decoder] {

      def apply[A](value: ConfigF[A]): Decoder[A] =
        Decoder.instance { c =>
          value match {
            case ConfigInt(n, v) => c.get[Int](n).map(v)
            case ConfigFlag(n, v) => c.get[Boolean](n).map(v)
            case ConfigPort(n, v) => c.get[Int](n).map(v)
            case ConfigServer(n, v) => c.get[String](n).map(v)
            case ConfigFile(n, v) => c.get[String](n).map(v)
            case ConfigSub(n, v) => c.downField(n).as(genDecode(v))
          }
        }
    })
  }
}
