package fi.vm.sade.utils.cas

import org.http4s.Status.Created
import org.http4s._
import org.http4s.client._
import org.http4s.dsl._
import org.http4s.headers.{Location, `Set-Cookie`}

import scala.xml._
import scalaz.concurrent.Task
import scalaz.stream.{Channel, async, channel}

object CasClient {
  type JSessionId = String
  type Username = String
  type TGTUrl = Uri
  type ST = String
}

class CasClient(virkailijaLoadBalancerUrl: Uri, client: Client) {
  import CasClient._

  def this(casServer: String, client: Client) = this(Uri.fromString(casServer).toOption.get, client)

  private val state = async.signalOf(Map[CasParams, JSessionId]())

  def sessionRefreshChannel: Channel[Task, CasParams, JSessionId] = channel.lift[Task, CasParams, JSessionId] { (casParams) =>
    refreshSession(casParams)
  }

  def casSessionChannel: Channel[Task, CasParams, JSessionId] = channel.lift(fetchSessionFromStore)

  private implicit val casUserEncoder = UrlForm.entityEncoder().contramap((user: CasUser) => UrlForm("username" -> user.username, "password" -> user.password))

  protected[cas] def getTicketGrantingTicket(params: CasParams): Task[TGTUrl] = {
    client
      .prepare(POST(resolve(virkailijaLoadBalancerUrl, uri("/cas/v1/tickets")), params.user))
      .flatMap(TicketGrantingTicketDecoder.decodeTgt)
  }

  def validateServiceTicket(service: String)(serviceTicket: ST): Task[Username] = {
    val pUri: Uri = resolve(virkailijaLoadBalancerUrl, uri("/cas/serviceValidate"))
      .withQueryParam("ticket", serviceTicket)
      .withQueryParam("service",service)

    client
      .prepare(GET(pUri))
      .flatMap(ServiceTicketResponseXmlDecoder.decodeUsername)
  }

  protected[cas] def getServiceTicket(service: Uri)(tgtUrl: TGTUrl) = {
    client
      .prepAs[ST](POST(tgtUrl, UrlForm("service" -> service.toString())))(ServiceTicketDecoder.stDecoder)
  }

  protected[cas] def getJSessionId(service: Uri)(serviceTicket: ST): Task[JSessionId] =
    client
      .prepare(GET(service.withQueryParam("ticket", serviceTicket)))
      .flatMap(JSESSIONDecoder.decodeJsession)

  protected[cas] def fetchCasSession(params: CasParams) = {
    val serviceUri = resolve(virkailijaLoadBalancerUrl, params.service.securityUri)

    (for (
      tgt <- getTicketGrantingTicket(params);
      st <- getServiceTicket(serviceUri)(tgt);
      session <- getJSessionId(serviceUri)(st)
    ) yield {
      session
    }).handle { case e@ParseException(pf) =>
      println(pf.details)
      throw e
    }
  }

  private def refreshSession(params: CasParams): Task[JSessionId] = fetchCasSession(params).flatMap((session) => state.compareAndSet {
    case None => Some(Map(params -> session))
    case Some(map) => Some(map.updated(params, session))
  }).map(_.get(params))

  private def getSession(params: CasParams): Task[Option[JSessionId]] = state.get.map(_.get(params))

  private def fetchSessionFromStore(params: CasParams): Task[JSessionId] = getSession(params).flatMap { case Some(session) => Task.now(session)
    case None => refreshSession(params)
  }
}

object ServiceTicketDecoder {
  import CasClient._

  val stPattern = "(ST-.*)".r
  val stDecoder = EntityDecoder.text.flatMapR[ST] { case stPattern(st) => DecodeResult.success(st)
    case nonSt => DecodeResult.failure(ParseFailure("Service Ticket decoding failed", s"response body is of wrong form ($nonSt)"))
  }
}

object TicketGrantingTicketDecoder {
  import CasClient.TGTUrl

  val tgtDecoder = EntityDecoder.decodeBy[TGTUrl](MediaRange.`*/*`) { (msg) =>
    val tgtPattern = "(.*TGT-.*)".r

    msg.headers.get(Location).map(_.value) match {
      case Some(tgtPattern(tgtUrl)) =>
        Uri.fromString(tgtUrl).fold((pf) => DecodeResult.failure(pf), (tgt) => DecodeResult.success(tgt))
      case Some(nontgturl) =>
        DecodeResult.failure(ParseFailure("TGT decoding failed", s"location header has wrong format $nontgturl"))
      case None =>
        DecodeResult.failure(ParseFailure("TGT decoding failed", "No location header"))
    }
  }
  val decodeTgt: (Response) => Task[TGTUrl] = response => DecodeResult.success(response).flatMap[TGTUrl] {
    case Created(resp) => tgtDecoder.decode(resp)
    case resp =>
      val body = resp.as[String].run
      DecodeResult.failure(ParseFailure("TGT decoding failed", s"invalid TGT creation status: ${resp.status.code}: $body"))
  }.fold(e => throw new ParseException(e), identity)
}

object JSESSIONDecoder {
  import CasClient._

  val jsessionDecoder = EntityDecoder.decodeBy[JSessionId](MediaRange.`*/*`) { (msg) =>
    msg.headers.collectFirst {
      case `Set-Cookie`(`Set-Cookie`(cookie)) if cookie.name == "JSESSIONID" => DecodeResult.success(cookie.content)
    }.getOrElse(DecodeResult.failure(ParseFailure("Decoding JSESSIONID failed", "no cookie found for JSESSIONID")))
  }

  def decodeJsession(response: Response) = DecodeResult.success(response).flatMap[JSessionId] {
    case resp if resp.status.isSuccess =>
      jsessionDecoder.decode(resp)
    case resp =>
      DecodeResult.failure(EntityDecoder.text.decode(resp).fold((_) => ParseFailure("Decoding JSESSIONID failed", s"service returned non-ok status code ${resp.status.code}"), (body) => ParseFailure("Decoding JSESSIONID failed", s"service returned non-ok status code ${resp.status.code}: $body")))
  }.fold(e => throw ParseException(e), identity)
}

object ServiceTicketResponseXmlDecoder {
  import CasClient._

  val serviceTicketDecoder =
    EntityDecoder.text.map(s => Utility.trim(scala.xml.XML.loadString(s))).flatMapR[Username] {
      case <cas:serviceResponse><cas:authenticationSuccess><cas:user>{user}</cas:user></cas:authenticationSuccess></cas:serviceResponse> => DecodeResult.success(user.text)
      case authenticationFailure => DecodeResult.failure(ParseFailure(s"Service Ticket validation response decoding failed ($authenticationFailure)", s"response body is of wrong form ($authenticationFailure)"))
    }

  def decodeUsername(response: Response) = DecodeResult.success(response).flatMap[Username] {
    case resp if resp.status.isSuccess =>
      serviceTicketDecoder.decode(resp)
    case resp =>
      DecodeResult.failure(EntityDecoder.text.decode(resp).fold((_) => ParseFailure("Decoding username failed", s"CAS returned non-ok status code ${resp.status.code}"), (body) => ParseFailure("Decoding username failed", s"CAS returned non-ok status code ${resp.status.code}: $body")))
  }.fold(e => throw ParseException(e), identity)
}