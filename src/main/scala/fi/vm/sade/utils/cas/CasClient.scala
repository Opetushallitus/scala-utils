package fi.vm.sade.utils.cas

import fi.vm.sade.utils.http.{HttpClient, DefaultHttpClient}
import fi.vm.sade.utils.slf4j.Logging

import scala.concurrent.duration._
import scala.xml.{Elem, XML}
import scalacache.ScalaCache
import scalacache.guava.GuavaCache
import scalacache.memoization._

/**
 * CAS client. Can validate a service ticket as well as fetch one from the CAS server.
 *
 * @param config  CAS server configuration
 */
class CasClient(config: CasConfig, httpClient: HttpClient = DefaultHttpClient) extends TicketClient with Logging {
  implicit val scalaCache = ScalaCache(GuavaCache())

  def validateServiceTicket(ticket: CasTicket): CasResponse = {
    def failure(error: String) = CasResponseFailure(error)
    def parseCasResponse(response: String) = {
      val responseXml: Elem = XML.loadString(response)
      (responseXml \\ "authenticationSuccess").theSeq match {
        case ((n: Elem) :: _) =>
          CasResponseSuccess((n \\ "user").text)
        case _ =>
          (responseXml \\ "authenticationFailure").theSeq match {
            case ((n: Elem) :: _) =>
              CasResponseFailure(n.attributes("code").map(_.text).headOption.getOrElse("CAS authentication failure"))
            case _ =>
              CasResponseFailure("Unexpected CAS error")
          }
      }
    }

    val casUrl: String = config.casRoot + "/serviceValidate"
    val (responseStatus, _, resultString) = httpClient.httpGet(casUrl).param("service", ticket.service).param("ticket", ticket.ticket)
      .responseWithHeaders()

    responseStatus match {
      case 200 => parseCasResponse(resultString)
      case _ => failure("CAS server at " + casUrl + " responded with status "+ responseStatus)
    }
  }

  def getServiceTicket(service: CasTicketRequest): Option[String] = {
    val casTicketUrl = config.casRoot + "/v1/tickets"

    def getTicketGrantingTicket(username: String, password: String): Option[String] = {
      val (responseStatus, headersMap, resultString) = httpClient.httpPost(casTicketUrl, None)
        .param("username", username)
        .param("password", password)
        .responseWithHeaders()

      responseStatus match {
        case 201 =>
          val ticketPattern = """.*/([^/]+)""".r
          val headerValue = headersMap.getOrElse("Location", "no location header")
          ticketPattern.findFirstMatchIn(headerValue) match {
            case Some(matched) => Some(matched.group(1))
            case None =>
              logger.warn("Successful ticket granting request, but no ticket found! Location header: " + headerValue)
              None
          }
        case _ =>
          logger.error("Invalid response status (" + responseStatus + ") from CAS server. Response body: " + resultString)
          None
      }
    }

    getTicketGrantingTicket(service.username, service.password).flatMap { ticket =>
      httpClient.httpPost(casTicketUrl + "/" + ticket, None)
        .param("service", service.service)
        .responseWithHeaders() match {
        case (status, _, ticketInResponse) if status >= 200 && status < 300 =>
          Some(ticketInResponse)
        case (status, _, body) =>
          logger.warn("Service ticket creation failed with response status " + status + ". Body: " + body)
          None
      }
    }
  }

  private def getCookies(service: CasTicketRequest, retryCount: Int = 0): List[String] = {
    def retry(count: Int, responseCode: Option[Int], headers: Map[String, String]): List[String] = {
      if (count < 3) {
        logger.warn(s"retrying getCookies, retry attempt #${count + 1}...")
        getCookies(service, count + 1)
      } else {
        throw CannotAuthenticateException(responseCode, headers)
      }
    }

    getServiceTicket(service) match {
      case Some(ticket) =>
        val (responseCode, headersMap, _) = httpClient.httpGet(service.service)
          .header("CasSecurityTicket", ticket)
          .responseWithHeaders()
        (responseCode, headersMap.get("Set-Cookie")) match {
          case (401, _) => retry(retryCount, Some(responseCode), headersMap)
          case (200, Some(cookies)) if cookies.contains("JSESSIONID") => cookies.split(", ").map(_.split(';').head).toList
          case (_, _) => retry(retryCount, Some(responseCode), headersMap)
        }
      case None => retry(retryCount, None, Map())
    }
  }

  def getSessionCookies(service: CasTicketRequest, createNewSession: Boolean = false, timeToLive: Duration = 15.minutes): List[String] =
    if (createNewSession)
      getCookies(service)
    else
      memoize(timeToLive) { getCookies(service) }
}