package fi.vm.sade.utils.cas

import fi.vm.sade.utils.cas.CasClient._
import org.http4s.Status.Created
import org.http4s._
import org.http4s.client._
import org.http4s.dsl._
import org.http4s.headers.{Location, `Set-Cookie`}

import scala.xml._
import scalaz.concurrent.Task

object CasClient {
  type JSessionId = String
  type Username = String
  type TGTUrl = Uri
  type ServiceTicket = String
}

/**
 *  Facade for establishing sessions with services protected by CAS, and also validating CAS service tickets.
 */
class CasClient(virkailijaLoadBalancerUrl: Uri, client: Client) {
  import CasClient._

  def this(casServer: String, client: Client) = this(Uri.fromString(casServer).toOption.get, client)

  def validateServiceTicket(service: String)(serviceTicket: ServiceTicket): Task[Username] = {
    ServiceTicketValidator.validateServiceTicket(virkailijaLoadBalancerUrl, client, service)(serviceTicket)
  }

  /**
   *  Establishes session with the requested service by
   *
   *  1) getting a CAS ticket granting ticket (TGT)
   *  2) getting a CAS service ticket
   *  3) getting a JSESSIONID cookie from the service.
   *
   *  Returns the JSESSIONID that can be used for communications later.
   */
  def fetchCasSession(params: CasParams): Task[JSessionId] = {
    val serviceUri = resolve(virkailijaLoadBalancerUrl, params.service.securityUri)

    for (
      tgt <- TicketGrantingTicketClient.getTicketGrantingTicket(virkailijaLoadBalancerUrl, client, params);
      st <- ServiceTicketClient.getServiceTicket(client, serviceUri)(tgt);
      session <- JSessionIdClient.getJSessionId(client, serviceUri)(st)
    ) yield {
      session
    }
  }
}

private[cas] object ServiceTicketValidator {
  def validateServiceTicket(virkailijaLoadBalancerUrl: Uri, client: Client, service: String)(serviceTicket: ServiceTicket): Task[Username] = {
    val pUri: Uri = resolve(virkailijaLoadBalancerUrl, uri("/cas/serviceValidate"))
      .withQueryParam("ticket", serviceTicket)
      .withQueryParam("service",service)

    client.fetch(GET(pUri)) {
      case r if r.status.isSuccess =>
        r.as[String].map(s => Utility.trim(scala.xml.XML.loadString(s))).map {
          case <cas:serviceResponse><cas:authenticationSuccess><cas:user>{user}</cas:user></cas:authenticationSuccess></cas:serviceResponse> =>
            user.text
          case authenticationFailure =>
            throw new CasClientException(s"Service Ticket validation response decoding failed at ${service}: response body is of wrong form ($authenticationFailure)")
        }
      case r => r.as[String].map {
        case body => throw new CasClientException(s"Decoding username failed at ${pUri}: CAS returned non-ok status code ${r.status.code}: $body")
      }
    }
  }
}

private[cas] object ServiceTicketClient {
  import CasClient._

  def getServiceTicket(client: Client, service: Uri)(tgtUrl: TGTUrl): Task[ServiceTicket] = {
    client.fetch[ServiceTicket](POST(tgtUrl, UrlForm("service" -> service.toString()))) {
      case r if r.status.isSuccess => r.as[String].map {
        case stPattern(st) => st
        case nonSt => throw new CasClientException(s"Service Ticket decoding failed at ${tgtUrl}: response body is of wrong form ($nonSt)")
      }
      case r => r.as[String].map {
        case body => throw new CasClientException(s"Service Ticket decoding failed at ${tgtUrl}: unexpected status ${r.status.code}: $body")
      }
    }
  }

  val stPattern = "(ST-.*)".r
}

private[cas] object TicketGrantingTicketClient extends Logging {
  import CasClient.TGTUrl
  val tgtPattern = "(.*TGT-.*)".r

  def getTicketGrantingTicket(virkailijaLoadBalancerUrl: Uri, client: Client, params: CasParams): Task[TGTUrl] = {
    val tgtUri: TGTUrl = resolve(virkailijaLoadBalancerUrl, uri("/cas/v1/tickets"))
    client.fetch(POST(tgtUri, UrlForm("username" -> params.user.username, "password" -> params.user.password))) {
      case Created(resp) =>
        val found: TGTUrl = resp.headers.get(Location).map(_.value) match {
          case Some(tgtPattern(tgtUrl)) =>
            Uri.fromString(tgtUrl).fold(
              (pf: ParseFailure) => throw new CasClientException(pf.message),
              (tgt) => tgt
            )
          case Some(nontgturl) =>
            throw new CasClientException(s"TGT decoding failed at ${tgtUri}: location header has wrong format $nontgturl")
          case None =>
            throw new CasClientException("TGT decoding failed at ${tgtUri}: No location header at")
        }
        Task.now(found)
      case r => r.as[String].map { body =>
        throw new CasClientException(s"TGT decoding failed at ${tgtUri}: invalid TGT creation status: ${r.status.code}: $body")
      }
    }
  }
}

private[cas] object JSessionIdClient {
  import CasClient._

  def getJSessionId(client: Client, service: Uri)(serviceTicket: ServiceTicket): Task[JSessionId] = {
    val sessionIdUri: Uri = service.withQueryParam("ticket", List(serviceTicket)).asInstanceOf[Uri]
    client.fetch(GET(sessionIdUri)) {
      case resp if resp.status.isSuccess =>
        Task.now(resp.headers.collectFirst {
          case `Set-Cookie`(`Set-Cookie`(cookie)) if cookie.name == "JSESSIONID" => cookie.content
        }.getOrElse(throw new CasClientException(s"Decoding JSESSIONID failed at ${sessionIdUri}: no cookie found for JSESSIONID")))

      case r => r.as[String].map { body =>
        throw new CasClientException(s"Decoding JSESSIONID failed at ${sessionIdUri}: service returned non-ok status code ${r.status.code}: $body")
      }
    }
  }
}

object CasLogout {
  def parseTicketFromLogoutRequest(logoutRequest: String): Option[String] = {
    Utility.trim(XML.loadString(logoutRequest)) match {
      case <samlp:LogoutRequest><saml:NameID>{nameID}</saml:NameID><samlp:SessionIndex>{ticket}</samlp:SessionIndex></samlp:LogoutRequest> =>
        Some(ticket.text)
      case _ => None
    }
  }
}

class CasClientException(message: String) extends RuntimeException(message)