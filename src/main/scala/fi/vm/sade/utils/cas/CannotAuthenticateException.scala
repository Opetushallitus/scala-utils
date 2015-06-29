package fi.vm.sade.utils.cas

case class CannotAuthenticateException(responseCode: Option[Int], headers: Map[String, String])
  extends Exception(s"cannot authenticate, response code $responseCode, headers: $headers")
