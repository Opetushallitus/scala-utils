package fi.vm.sade.utils.cas

import java.util.concurrent.TimeUnit

import fi.vm.sade.utils.http.{HttpClient, HttpRequest}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.Duration
import scalaj.http.HttpOptions.HttpOption

@RunWith(classOf[JUnitRunner])
class CasClientSpec extends Specification {
  def createMock() = new Mocks(
    Map(
      ("POST", "http://localhost/cas/v1/tickets") ->
        MockResponse(201, Map("Location" -> "http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo"), ""),

      ("POST", "http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo") ->
        MockResponse(200, Map(), "", Some("ST-63529-TQpkXpIE0YgGXFIwTHaj-cas.foo")),

      ("GET", "http://localhost/authentication-service/j_spring_cas_security_check") ->
        MockResponse(200, Map("Set-Cookie" -> "JSESSIONID=9C16A50F8E5DE52D03F237CB3500D3A8; Path=/authentication-service/; HttpOnly, route_authentication=c23b64de9030d9a60763b124abce01e4db5e5ae0; Path=/authentication-service/"), ""),

      ("GET", "http://localhost/authentication-service/resources/s2s/byHetu/111111-1975") ->
        MockResponse(200, Map("Content-Type" -> "application/json"), "{\"oidHenkilo\":\"oid\",\"kutsumanimi\":\"full\",\"sukunimi\":\"name\"}")
    )
  )

  "CasClient" should {
    val casConfig = CasConfig("http://localhost/cas")

    "get session cookies for requested service" in {
      new CasClient(casConfig, createMock()).getSessionCookies(CasTicketRequest(
        service = "http://localhost/authentication-service/j_spring_cas_security_check",
        username = "foo",
        password = "bar"
      )) must_== List("JSESSIONID=9C16A50F8E5DE52D03F237CB3500D3A8", "route_authentication=c23b64de9030d9a60763b124abce01e4db5e5ae0")
    }

    "cache session cookies for multiple requests" in {
      val mock = createMock()
      val casClient = new CasClient(casConfig, mock)
      val ticketRequest = CasTicketRequest(
        service = "http://localhost/authentication-service/j_spring_cas_security_check",
        username = "foo",
        password = "bar"
      )

      casClient.getSessionCookies(ticketRequest)

      casClient.getSessionCookies(ticketRequest)

      mock.requests("http://localhost/cas/v1/tickets") must_== 1
      mock.requests("http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo") must_== 1
    }

    "get new session when the old one is timed out" in {
      val mock = createMock()
      val casClient = new CasClient(casConfig, mock)
      val ticketRequest = CasTicketRequest(
        service = "http://localhost/authentication-service/j_spring_cas_security_check",
        username = "foo",
        password = "bar"
      )

      casClient.getSessionCookies(ticketRequest, timeToLive = Duration(100, TimeUnit.MILLISECONDS))

      Thread.sleep(200)

      casClient.getSessionCookies(ticketRequest, timeToLive = Duration(100, TimeUnit.MILLISECONDS))

      mock.requests("http://localhost/cas/v1/tickets") must_== 2
      mock.requests("http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo") must_== 2
    }

    "get new session when requested" in {
      val mock = createMock()
      val casClient = new CasClient(casConfig, mock)
      val ticketRequest = CasTicketRequest(
        service = "http://localhost/authentication-service/j_spring_cas_security_check",
        username = "foo",
        password = "bar"
      )

      casClient.getSessionCookies(ticketRequest)

      casClient.getSessionCookies(ticketRequest, createNewSession = true)

      mock.requests("http://localhost/cas/v1/tickets") must_== 2
      mock.requests("http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo") must_== 2
    }
  }
}


class Mocks(mockedRequests: Map[(String, String), HttpRequest]) extends HttpClient {
  var requests: Map[String, Int] = Map()
  private def incCount(url: String) = synchronized {
    requests = requests + (url -> (requests.getOrElse(url, 0) + 1))
  }
  override def httpGet(url: String): HttpRequest = {
    incCount(url)
    mockedRequests(("GET", url))
  }
  override def httpGet(url: String, options: HttpOption*): HttpRequest = {
    incCount(url)
    mockedRequests(("GET", url))
  }
  override def httpPost(url: String, data: Option[String]): HttpRequest = {
    incCount(url)
    mockedRequests("POST", url)
  }
  override def httpPost(url: String, data: Option[String], options: HttpOption*): HttpRequest = {
    incCount(url)
    mockedRequests("POST", url)
  }
  override def httpPut(url: String): HttpRequest = ???
  override def httpPut(url: String, options: HttpOption*): HttpRequest = ???
}


case class MockResponse(responseCode: Int, headers: Map[String, String], body: String, resp: Option[String] = None) extends HttpRequest {
  override def responseWithHeaders(): (Int, Map[String, String], String) = (responseCode, headers, body)
  override def header(key: String, value: String): HttpRequest = this
  override def param(key: String, value: String): HttpRequest = this
  override def response(): Option[String] = resp
  override def getUrl: String = ""
}