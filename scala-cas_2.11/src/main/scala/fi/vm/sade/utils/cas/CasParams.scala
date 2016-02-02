package fi.vm.sade.utils.cas

import org.http4s.dsl._
import org.http4s.{ParseException, Uri}

case class CasUser(username: String, password: String)

case class CasService(securityUri:Uri)

case class CasParams(service: CasService, user: CasUser) {

}

object CasParams {

  def apply(service:String, username: String, password: String):CasParams =
    Uri.fromString(ensureTailingSlash(service)).fold((e) =>
      throw ParseException(e), (service: Uri) =>
      CasParams.apply(CasService(resolve(service, uri("j_spring_cas_security_check"))),
        CasUser(username, password)))
  private def ensureTailingSlash(service:String) = service.last match {
    case '/' => service
    case _ => service + "/"
  }


}