package fi.vm.sade.utils.cas

import fi.vm.sade.utils.cas.CasClient._
import org.http4s.EntityDecoder.collectBinary
import org.http4s.Status.Created
import org.http4s._
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
  val textOrXmlDecoder = EntityDecoder.decodeBy(MediaRange.`text/*`, MediaType.`application/xml`)(msg =>
    collectBinary(msg).map(bs => new String(bs.toArray, msg.charset.getOrElse(DefaultCharset).nioCharset))
  )
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
        getServiceTicket(params, serviceUri)
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

    client
      .fetch(GET(pUri))(decodeUsername)
  }

  private val serviceTicketDecoder =
    textOrXmlDecoder.map(s => Utility.trim(scala.xml.XML.loadString(s))).flatMapR[Username] {
      case <cas:serviceResponse><cas:authenticationSuccess><cas:user>{user}</cas:user></cas:authenticationSuccess></cas:serviceResponse> => DecodeResult.success(user.text)
      case authenticationFailure => DecodeResult.failure(InvalidMessageBodyFailure(s"Service Ticket validation response decoding failed: response body is of wrong form ($authenticationFailure)"))
    }

  private def decodeUsername(response: Response) = {
    DecodeResult.success(response).flatMap[Username] {
      case resp if resp.status.isSuccess =>
        serviceTicketDecoder.decode(resp, true)
      case resp =>
        DecodeResult.failure(textOrXmlDecoder.decode(resp, true).fold(
          (_) => InvalidMessageBodyFailure(s"Decoding username failed: CAS returned non-ok status code ${resp.status.code}"),
          (body) => InvalidMessageBodyFailure(s"Decoding username failed: CAS returned non-ok status code ${resp.status.code}: $body"))
        )
    }.fold(e => throw new CasClientException(e.message), identity)
  }
}

private[cas] object ServiceTicketClient {
  import CasClient._

  def getServiceTicketFromTgt(client: Client, service: Uri)(tgtUrl: TGTUrl) = {
    client.fetchAs[ServiceTicket](POST(tgtUrl, UrlForm("service" -> service.toString())))(stDecoder)
  }

  val stPattern = "(ST-.*)".r
  val stDecoder = textOrXmlDecoder.flatMapR[ServiceTicket] {
    case stPattern(st) => DecodeResult.success(st)
    case nonSt => DecodeResult.failure(InvalidMessageBodyFailure(s"Service Ticket decoding failed: response body is of wrong form ($nonSt)"))
  }
}

private[cas] object TicketGrantingTicketClient extends Logging {
  import CasClient.TGTUrl

  def getTicketGrantingTicket(virkailijaLoadBalancerUrl: Uri, client: Client, params: CasParams): Task[TGTUrl] = {
    client
      .fetch(POST(resolve(virkailijaLoadBalancerUrl, uri("/cas/v1/tickets")), params.user)(casUserEncoder))(decodeTgt)
  }

  private val casUserEncoder = UrlForm.entityEncoder().contramap((user: CasUser) => UrlForm("username" -> user.username, "password" -> user.password))

  private val tgtDecoder = EntityDecoder.decodeBy[TGTUrl](MediaRange.`*/*`) { (msg) =>
    val tgtPattern = "(.*TGT-.*)".r

    msg.headers.get(Location).map(_.value) match {
      case Some(tgtPattern(tgtUrl)) =>
        Uri.fromString(tgtUrl).fold(
          (pf: ParseFailure) => DecodeResult.failure(InvalidMessageBodyFailure(pf.message)),
          (tgt) => DecodeResult.success(tgt)
        )
      case Some(nontgturl) =>
        DecodeResult.failure(InvalidMessageBodyFailure(s"TGT decoding failed: location header has wrong format $nontgturl"))
      case None =>
        DecodeResult.failure(InvalidMessageBodyFailure("TGT decoding failed: No location header"))
    }
  }
  private val decodeTgt: (Response) => Task[TGTUrl] = {response =>
    DecodeResult.success(response)
      .flatMap[TGTUrl] {
      case Created(resp) =>
        tgtDecoder.decode(resp, true)
      case resp =>
        val body = resp.as[String].run
        DecodeResult.failure(InvalidMessageBodyFailure(s"TGT decoding failed: invalid TGT creation status: ${resp.status.code}: $body"))
    }
      .fold(e => throw new CasClientException(e.message), identity)
  }
}

private[cas] object SessionCookieClient {
  import CasClient._

  def getSessionCookieValue(client: Client, service: Uri, sessionCookieName: String)(serviceTicket: ServiceTicket): Task[SessionCookie] = {
    val uriWithQueryParam: Uri = service.withQueryParam("ticket", List(serviceTicket)).asInstanceOf[Uri]
    client.fetch(GET(uriWithQueryParam))(decodeJsession(sessionCookieName, _))
  }

  private def jsessionDecoder(sessionCookieName: String) = EntityDecoder.decodeBy[SessionCookie](MediaRange.`*/*`) { (msg) =>
    msg.headers.collectFirst {
      case `Set-Cookie`(`Set-Cookie`(cookie)) if cookie.name == sessionCookieName => DecodeResult.success(cookie.content)
    }.getOrElse(DecodeResult.failure(InvalidMessageBodyFailure(s"Decoding $sessionCookieName failed: no cookie found for JSESSIONID")))
  }

  private def decodeJsession(sessionCookieName: String, response: Response) = DecodeResult.success(response).flatMap[SessionCookie] {
    case resp if resp.status.isSuccess =>
      jsessionDecoder(sessionCookieName).decode(resp, true)
    case resp =>
      DecodeResult.failure(textOrXmlDecoder.decode(resp, true).fold(
        (_) => InvalidMessageBodyFailure(s"Decoding $sessionCookieName faile: service returned non-ok status code ${resp.status.code}"),
        (body) => InvalidMessageBodyFailure(s"Decoding $sessionCookieName failed: service returned non-ok status code ${resp.status.code}: $body"))
      )
  }.fold(e => throw new CasClientException(e.message), identity)
}



class CasClientException(message: String) extends RuntimeException(message)
