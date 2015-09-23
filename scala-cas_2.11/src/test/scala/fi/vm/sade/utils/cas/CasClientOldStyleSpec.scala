package fi.vm.sade.utils.cas

import org.http4s._
import org.http4s.client._
import org.http4s.dsl._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scalaz.concurrent.Task

@RunWith(classOf[JUnitRunner])
class CasClientOldStyleSpec extends Specification {
  def createMock() = new Mocks(

      (POST, "http://localhost/cas/v1/tickets") ->
        MockResponse(201, Map("Location" -> "http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo")),

      (POST, "http://localhost/cas/v1/tickets/TGT-63528-7e6K4Ft6YCbiiLFdjk7Y-cas.foo") ->
        MockResponse(200, Map(), Some("ST-63529-TQpkXpIE0YgGXFIwTHaj-cas.foo")),

      (GET, "http://localhost/authentication-service/j_spring_cas_security_check?ticket=ST-63529-TQpkXpIE0YgGXFIwTHaj-cas.foo") ->
        MockResponse(200, Map("Set-Cookie" -> "JSESSIONID=9C16A50F8E5DE52D03F237CB3500D3A8; Path=/authentication-service/; HttpOnly")),

      (GET, "http://localhost/authentication-service/resources/s2s/byHetu/111111-1975") ->
        MockResponse(200, Map("Content-Type" -> "application/json"), Some("{\"oidHenkilo\":\"oid\",\"kutsumanimi\":\"full\",\"sukunimi\":\"name\"}"))

  )

  "CasClient" should {
    val params = CasParams("/authentication-service", "foo", "bar")

    "get session cookies for requested service" in {
      new CasClient(uri("http://localhost"), createMock()).fetchCasSession(params).run must_== "9C16A50F8E5DE52D03F237CB3500D3A8"
    }

    /*"cache session cookies for multiple requests" in {
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
    } */
  }
}


class Mocks(mockedRequests: ((Method, String), MockResponse)*) extends Client {
  var requests: Map[String, Int] = Map()
  private def incCount(url: String) = synchronized {
    requests = requests + (url -> (requests.getOrElse(url, 0) + 1))

  }

  val requestMap = mockedRequests.toMap

  override def shutdown(): Task[Unit] = Task.now[Unit]{}

  override def prepare(req: Request): Task[Response] = {
    incCount(req.uri.toString())
    requestMap.get((req.method, req.uri.toString())).map(_.toResponse).getOrElse{
      NotFound()}

  }
}


case class MockResponse(responseCode: Int, headermap: Map[String, String], body: Option[String] = None) {

  def status = Status.fromInt(responseCode).fold(e => throw ParseException(e), identity)

  def headers = Headers(for(
    (key, value) <- headermap.toList
  ) yield Header(key, value))

  def toResponse: Task[Response] = {
    val rawResp = Response(status, headers = headers)
    if (body.isDefined)
      rawResp.withBody(body.get)
    else
      Task.now(rawResp)
  }


}