package com.rallyhealth.examples.confree

object help {
  import algebra._
  import cats.arrow.FunctionK
  import cats.data.State
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import dsl.Dsl

  case class HelpState(
    help: String = "",
    indent: Int = 0
  ) {

    def -->(h: String): HelpState =
      copy(help = help + (0 until indent * 2).foldLeft[String]("")((a, _) => a + " ") + h + "\n")

    def indented: HelpState = copy(indent = indent + 1)
    def dedented: HelpState = copy(indent = indent - 1)
  }

  def genHelp[X](config: Dsl[X]): String = {
    type G[A] = State[HelpState, A]

    def genHelp0[Y](config: Dsl[Y]): G[Y] = {
      config.foldMap(new FunctionK[ConfigF, G] {
        override def apply[A](value: ConfigF[A]): G[A] = value match {
          case ConfigInt(n, v) => State.modify[HelpState](_ --> (n + "\t - an integer")) *> v(0).pure[G]
          case ConfigFlag(n, v) => State.modify[HelpState](_ --> (n + "\t - a boolean flag")) *> v(false).pure[G]
          case ConfigPort(n, v) => State.modify[HelpState](_ --> (n + "\t - a port number")) *> v(0).pure[G]
          case ConfigServer(n, v) => State.modify[HelpState](_ --> (n + "\t - a server address")) *> v("").pure[G]
          case ConfigFile(n, v) => State.modify[HelpState](_ --> (n + "\t - a file path")) *> v("").pure[G]
          case ConfigSub(n, v) =>
            for {
              _ <- State.modify[HelpState](_ --> (n + "\t - a sub-configuration"))
              _ <- State.modify[HelpState](_.indented)
              a <- genHelp0(v)
              _ <- State.modify[HelpState](_.dedented)
            } yield a
        }
      })
    }

    genHelp0(config).runS(HelpState()).value.help
  }
}
