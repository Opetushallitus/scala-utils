package fi.vm.sade.utils.cas

import fi.vm.sade.utils.http.DefaultHttpClient
import fi.vm.sade.utils.slf4j.Logging

import scala.xml.{Elem, XML}

/**
 * CAS client. Can validate a service ticket as well as fetch one from the CAS server.
 *
 * @param config  CAS server configuration
 */
class CasClient(config: CasConfig) extends TicketClient with Logging {
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
    val (responseCode, headers, resultString) = DefaultHttpClient.httpGet(casUrl).param("service", ticket.service).param("ticket", ticket.ticket)
      .responseWithHeaders

    responseCode match {
      case 200 => parseCasResponse(resultString)
      case _ => failure("CAS server at " + casUrl + " responded with status "+ responseCode)
    }
  }

  def getServiceTicket(service: CasTicketRequest): Option[String] = {
    val casTicketUrl = config.casRoot + "/v1/tickets"

    def getTicketGrantingTicket(username: String, password: String): Option[String] = {
      val (responseCode, headersMap, resultString) = DefaultHttpClient.httpPost(casTicketUrl, None)
        .param("username", username)
        .param("password", password)
        .responseWithHeaders

      responseCode match {
        case 201 => {
          val ticketPattern = """.*/([^/]+)""".r
          val headerValue = headersMap.getOrElse("Location",List("no location header")).head
          ticketPattern.findFirstMatchIn(headerValue) match {
            case Some(matched) => Some(matched.group(1))
            case None => {
              logger.warn("Successful ticket granting request, but no ticket found! Location header: " + headerValue)
              None
            }
          }
        }
        case _ => {
          logger.error("Invalid response code (" + responseCode + ") from CAS server. Response body: " + resultString)
          None
        }
      }
    }

    getTicketGrantingTicket(service.username, service.password).flatMap { ticket =>
      DefaultHttpClient.httpPost(casTicketUrl + "/" + ticket, None)
        .param("service", service.service)
        .responseWithHeaders match {
        case (code, _, ticket) if (code >= 200 && code < 300) =>
          Some(ticket)
        case (code, _, body) =>
          logger.warn("Service ticket creation failed with response code " + code + ". Body: " + body)
          None
      }
    }
  }
}