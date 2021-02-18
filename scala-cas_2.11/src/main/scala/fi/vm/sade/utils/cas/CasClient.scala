package fi.vm.sade.utils.cas

import org.http4s.EntityDecoder.collectBinary
import org.http4s.Status.Created
import org.http4s._
import org.http4s.client._
import org.http4s.dsl._
import org.http4s.headers.{Location, `Set-Cookie`}

import scala.xml._
import scalaz.concurrent.Task
import scalaz.{-\/, \/-}

import scala.util.{Failure, Success, Try}

object CasClient {
  type SessionCookie = String
  type Username = String
  type OppijaAttributes = Map[String, String]
  type TGTUrl = Uri
  type ServiceTicket = String
  val textOrXmlDecoder = EntityDecoder.decodeBy(MediaRange.`text/*`, MediaType.`application/xml`)(msg =>
    collectBinary(msg).map(bs => new String(bs.toArray, msg.charset.getOrElse(DefaultCharset).nioCharset))
  )
}

/**
 *  Facade for establishing sessions with services protected by CAS, and also validating CAS service tickets.
 */
class CasClient(casBaseUrl: Uri, client: Client, callerId: String) extends Logging {
  import CasClient._

  def this(casServer: String, client: Client, callerId: String) = this(Uri.fromString(casServer).toOption.get, client, callerId)

  def validateServiceTicketWithOppijaAttributes(service: String)(serviceTicket: ServiceTicket): Task[OppijaAttributes] = {
    validateServiceTicket[OppijaAttributes](casBaseUrl, client, callerId, service, decodeOppijaAttributes)(serviceTicket)
  }

  def validateServiceTicketWithVirkailijaUsername(service: String)(serviceTicket: ServiceTicket): Task[Username] = {
    validateServiceTicket[Username](casBaseUrl, client, callerId, service, decodeVirkailijaUsername)(serviceTicket)
  }

  def validateServiceTicket[R](service: String)(serviceTicket: ServiceTicket, responseHandler: Response => Task[R]): Task[R] = {
    validateServiceTicket[R](casBaseUrl, client, callerId, service, responseHandler)(serviceTicket)
  }

  private def validateServiceTicket[R](casBaseUrl: Uri, client: Client, callerId: String, service: String, responseHandler: Response => Task[R])(serviceTicket: ServiceTicket): Task[R] = {
    val pUri: Uri = casBaseUrl.withPath(casBaseUrl.path + "/serviceValidate")
      .withQueryParam("ticket", serviceTicket)
      .withQueryParam("service",service)

    val task = GET(pUri)
    FetchHelper.fetch[R](client, callerId: String, task, responseHandler)
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
    val serviceUri = resolve(casBaseUrl, params.service.securityUri)

    for (
      st <- getServiceTicketWithRetryOnce(params, serviceUri);
      session <- SessionCookieClient.getSessionCookieValue(client, serviceUri, sessionCookieName, callerId)(st)
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
        retryServiceTicket(params, serviceUri, callerId)
    }
  }

  private def retryServiceTicket(params: CasParams, serviceUri: TGTUrl, callerId: String): Task[ServiceTicket] = {
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
      tgt <- TicketGrantingTicketClient.getTicketGrantingTicket(casBaseUrl, client, params, callerId);
      st <- ServiceTicketClient.getServiceTicketFromTgt(client, serviceUri, callerId)(tgt)
    ) yield {
      st
    }
  }

  private val oppijaServiceTicketDecoder: EntityDecoder[OppijaAttributes] = textOrXmlDecoder
    .map(s => Utility.trim(scala.xml.XML.loadString(s)))
    .flatMapR[OppijaAttributes] { serviceResponse =>
      Try {
        val attributes: NodeSeq = (serviceResponse \ "authenticationSuccess" \ "attributes")

        List("mail", "clientName", "displayName", "givenName", "personOid", "personName", "firstName", "nationalIdentificationNumber",
        "impersonatorNationalIdentificationNumber", "impersonatorDisplayName")
          .map(key => (key, (attributes \ key).text))
          .toMap
      } match {
        case Success(decoded) => DecodeResult.success(decoded)
        case Failure(ex) => DecodeResult.failure(InvalidMessageBodyFailure("Oppija Service Ticket validation response decoding failed: Failed to parse required values from response body", Some(ex)))
      }
    }

  private val virkailijaServiceTicketDecoder: EntityDecoder[Username] = textOrXmlDecoder
    .map(s => Utility.trim(scala.xml.XML.loadString(s)))
    .flatMapR[Username] {
      case <cas:serviceResponse><cas:authenticationSuccess><cas:user>{user}</cas:user></cas:authenticationSuccess></cas:serviceResponse> => DecodeResult.success(user.text)
      case authenticationFailure => DecodeResult.failure(InvalidMessageBodyFailure(s"Virkailija Service Ticket validation response decoding failed: response body is of wrong form ($authenticationFailure)"))
    }

  private val casFailure = (debugLabel: String, resp: Response) => {
    textOrXmlDecoder
      .decode(resp, true)
      .fold(
        (_) => InvalidMessageBodyFailure(s"Decoding $debugLabel failed: CAS returned non-ok status code ${resp.status.code}"),
        (body) => InvalidMessageBodyFailure(s"Decoding $debugLabel failed: CAS returned non-ok status code ${resp.status.code}: $body"))
  }

  /**
   * Decode CAS Oppija's service ticket validation response to various oppija attributes.
   */
  def decodeOppijaAttributes: (Response) => Task[OppijaAttributes] = { response =>
    decodeCASResponse[OppijaAttributes](response, "oppija attributes", oppijaServiceTicketDecoder)
  }

  /**
   * Decode CAS Virkailija's service ticket validation response to username.
   */
  def decodeVirkailijaUsername: (Response) => Task[Username] = { response =>
    decodeCASResponse[Username](response, "username", virkailijaServiceTicketDecoder)
  }

  private def decodeCASResponse[R](response: Response, debugLabel: String, decoder: EntityDecoder[R]): Task[R] = {
    DecodeResult.success(response)
      .flatMap[R] {
        case resp if resp.status.isSuccess => decoder.decode(resp, true)
        case resp                          => DecodeResult.failure(casFailure.apply(debugLabel, resp))
      }.fold(e => throw new CasClientException(e.message), identity)
  }
}

private[cas] object ServiceTicketClient {
  import CasClient._

  def getServiceTicketFromTgt(client: Client, service: Uri, callerId: String)(tgtUrl: TGTUrl) = {
    val task = POST(tgtUrl, UrlForm("service" -> service.toString()))
    val handler = stDecoder
    FetchHelper.fetchAs(client, callerId, task, handler)
  }

  val stPattern = "(ST-.*)".r
  val stDecoder = textOrXmlDecoder.flatMapR[ServiceTicket] {
    case stPattern(st) => DecodeResult.success(st)
    case nonSt => DecodeResult.failure(InvalidMessageBodyFailure(s"Service Ticket decoding failed: response body is of wrong form ($nonSt)"))
  }
}

private[cas] object TicketGrantingTicketClient extends Logging {
  import CasClient.TGTUrl

  def getTicketGrantingTicket(casBaseUrl: Uri, client: Client, params: CasParams, callerId: String): Task[TGTUrl] = {
    val task = POST(casBaseUrl.withPath(casBaseUrl.path + "/v1/tickets"), params.user)(casUserEncoder)
    FetchHelper.fetch(client, callerId, task, decodeTgt)
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

  def getSessionCookieValue(client: Client, service: Uri, sessionCookieName: String, callerId: String)(serviceTicket: ServiceTicket): Task[SessionCookie] = {
    val uriWithQueryParam: Uri = service.withQueryParam("ticket", List(serviceTicket)).asInstanceOf[Uri]
    val task = GET(uriWithQueryParam)
    FetchHelper.fetch(client = client, callerId = callerId, task = task, handler = decodeJsession(sessionCookieName, _))
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

private object FetchHelper {
  private def addDefaultHeaders(task: Task[Request], callerId: String): Task[Request] = {
    task.putHeaders(
      Header("Caller-Id", callerId),
      Header("CSRF", callerId)
    )
  }

  def fetch[A](client: Client, callerId: String, task: Task[Request], handler: Response => Task[A]): Task[A] = {
    val taskWithHeaders: Task[Request] = addDefaultHeaders(task, callerId)
    client.fetch(taskWithHeaders)(handler)
  }

  def fetchAs[A](client: Client, callerId: String, task: Task[Request], decoder: EntityDecoder[A]): Task[A] = {
    val taskWithHeaders: Task[Request] = addDefaultHeaders(task, callerId)
    client.fetchAs[A](taskWithHeaders)(decoder)
  }
}


class CasClientException(message: String) extends RuntimeException(message)
