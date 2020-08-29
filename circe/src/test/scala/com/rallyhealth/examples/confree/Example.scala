package com.rallyhealth.examples.confree

object Example {
  import cats.syntax.apply._
  import dsl._
  import io.circe.literal._

  case class AuthConfig(
    port: Int,
    host: String
  )
  case class ServerConfig(
    logging: Boolean,
    auth: AuthConfig
  )

  def main(args: Array[String]): Unit = {

    val authConfig = (int("port"), server("host")).mapN(AuthConfig)
    val serverConfig = (flag("logging"), sub("auth")(authConfig)).mapN(ServerConfig)

    val serverConfigDecode = json.genDecode(serverConfig)
    val serverConfigHelp = help.genHelp(serverConfig)
    println("HELP:\n" + serverConfigHelp)

    val serverConfigJson = json"""
      {
        "logging": true,
        "auth": {
          "port": 2020,
          "host": "localhost"
        }
      }
    """

    val serverConfigDecoded = serverConfigJson.as(serverConfigDecode)
    println("DECODED: " + serverConfigDecoded)
  }
}
