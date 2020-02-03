package fi.vm.sade.utils.cas

import fi.vm.sade.utils.cas.CasClient._
import org.http4s.Status.Created
import org.http4s.{Response, _}
import org.http4s.client._
import org.http4s.dsl._
import org.http4s.headers.{Location, `Set-Cookie`}

import scala.xml._
import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

object CasClient {
  type SessionCookie = String
  type Username = String
  type TGTUrl = Uri
  type ServiceTicket = String
}

/**
 *  Facade for establishing sessions with services protected by CAS, and also validating CAS service tickets.
 */
class CasClient(virkailijaLoadBalancerUrl: Uri, client: Client) extends Logging {
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
   *  3) getting a session cookie from the service.
   *
   *  Returns the session that can be used for communications later.
   */
  def fetchCasSession(params: CasParams, sessionCookieName: String = "JSESSIONID"): Task[SessionCookie] = {
    val serviceUri = resolve(virkailijaLoadBalancerUrl, params.service.securityUri)

    for (
      st <- getServiceTicketWithRetryOnce(params, serviceUri);
      session <- SessionCookieClient.getSessionCookieValue(client, serviceUri, sessionCookieName)(st)
    ) yield {
      session
    }
  }

  private def getServiceTicketWithRetryOnce(params: CasParams, serviceUri: TGTUrl): Task[ServiceTicket] = {
    getServiceTicket(params, serviceUri).attempt.flatMap {
      case \/-(success) =>
        Task(success)
      case -\/(throwable) =>
        logger.warn("Fetching TGT or ST failed. Retrying once (and only once) in case the error was ephemeral.", throwable)
        retryServiceTicket(params, serviceUri)
    }
  }

  private def retryServiceTicket(params: CasParams, serviceUri: TGTUrl): Task[ServiceTicket] = {
    getServiceTicket(params, serviceUri).attempt.map {
      case \/-(retrySuccess) =>
        logger.info("Fetching TGT and ST was successful after one retry.")
        retrySuccess
      case -\/(retryThrowable) =>
        logger.error("Fetching TGT or ST failed also after one retry.", retryThrowable)
        throw retryThrowable
    }
  }

  private def getServiceTicket(params: CasParams, serviceUri: TGTUrl): Task[ServiceTicket] = {
    for (
      tgt <- TicketGrantingTicketClient.getTicketGrantingTicket(virkailijaLoadBalancerUrl, client, params);
      st <- ServiceTicketClient.getServiceTicketFromTgt(client, serviceUri)(tgt)
    ) yield {
      st
    }
  }
}

private[cas] object ServiceTicketValidator {
  def validateServiceTicket(virkailijaLoadBalancerUrl: Uri, client: Client, service: String)(serviceTicket: ServiceTicket): Task[Username] = {
    val pUri: Uri = resolve(virkailijaLoadBalancerUrl, uri("/cas/serviceValidate"))
      .withQueryParam("ticket", serviceTicket)
      .withQueryParam("service",service)

    val task = GET(pUri)

    def handler(response: Response): Task[Username] = {
      response match {
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

    FetchHelper.fetch(client, task, handler)
  }
}

private[cas] object ServiceTicketClient {
  import CasClient._

  def getServiceTicketFromTgt(client: Client, service: Uri)(tgtUrl: TGTUrl): Task[ServiceTicket] = {
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
    val task = POST(tgtUri, UrlForm("username" -> params.user.username, "password" -> params.user.password))

    def handler(response: Response): Task[TGTUrl] = {
      response match {
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
    FetchHelper.fetch(client, task, handler)
  }
}

private[cas] object SessionCookieClient {
  import CasClient._

  def getSessionCookieValue(client: Client, service: Uri, sessionCookieName: String)(serviceTicket: ServiceTicket): Task[SessionCookie] = {
    val sessionIdUri: Uri = service.withQueryParam("ticket", List(serviceTicket)).asInstanceOf[Uri]
    val task = GET(sessionIdUri)

    def handler(response: Response): Task[SessionCookie] = {
      response match {
        case resp if resp.status.isSuccess =>
          Task.now(resp.headers.collectFirst {
            case `Set-Cookie`(`Set-Cookie`(cookie)) if cookie.name == sessionCookieName => cookie.content
          }.getOrElse(throw new CasClientException(s"Decoding $sessionCookieName failed at ${sessionIdUri}: no cookie found for JSESSIONID")))

        case r => r.as[String].map { body =>
          throw new CasClientException(s"Decoding $sessionCookieName failed at ${sessionIdUri}: service returned non-ok status code ${r.status.code}: $body")
        }
      }
    }

    FetchHelper.fetch(client, task, handler)
  }
}

private object FetchHelper {
  private val callerId = "scala-utils.scala-cas_2.12"
  private val defaultHeaders: Header = Header("Caller-Id", callerId)

  def fetch[A](client: Client, task: Task[Request], handler: Response => Task[A]): Task[A] = {
    val taskWithHeaders = task.putHeaders(defaultHeaders)
    client.fetch(taskWithHeaders)(handler)
  }
}

class CasClientException(message: String) extends RuntimeException(message)
