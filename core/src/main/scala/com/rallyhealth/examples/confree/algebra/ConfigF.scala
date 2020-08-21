package com.rallyhealth.examples.confree.algebra

import cats.free.FreeApplicative

sealed trait ConfigF[A]

case class ConfigInt[A](
  field: String,
  value: Int => A
) extends ConfigF[A]

case class ConfigFlag[A](
  field: String,
  value: Boolean => A
) extends ConfigF[A]

case class ConfigPort[A](
  field: String,
  value: Int => A
) extends ConfigF[A]

case class ConfigServer[A](
  field: String,
  value: String => A
) extends ConfigF[A]

case class ConfigFile[A](
  field: String,
  value: String => A
) extends ConfigF[A]

case class ConfigSub[A](
  field: String,
  value: FreeApplicative[ConfigF, A]
) extends ConfigF[A]
