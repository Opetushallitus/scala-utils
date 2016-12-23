package fi.vm.sade.utils.cas

import org.http4s.{ParseFailure, Uri}
import org.http4s.dsl._

case class CasUser(username: String, password: String)

case class CasService(securityUri:Uri)

case class CasParams(service: CasService, user: CasUser) {
  override def toString = service.securityUri.toString
}

object CasParams {
  def apply(service:String, username: String, password: String):CasParams ={
    Uri.fromString(ensureTrailingSlash(service)).fold(
      (e: ParseFailure) => throw new IllegalArgumentException(e),
      (service: Uri) => CasParams(CasService(resolve(service, uri("j_spring_cas_security_check"))), CasUser(username, password)))
  }

  private def ensureTrailingSlash(service:String) = service.last match {
    case '/' => service
    case _ => service + "/"
  }
}