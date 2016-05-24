package fi.vm.sade.utils.cas

import org.http4s._
import org.http4s.client.{Client, DisposableResponse}
import org.http4s.dsl._
import org.http4s.headers.`Content-Type`
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scalaz.concurrent.Task

@RunWith(classOf[JUnitRunner])
class CasAbleClientSpec extends FlatSpec with Matchers {

  val virkailijaUri: Uri = uri("https://localhost")

  behavior of "CasAbleClient"

  it should "inject a CAS session into a http request as a JSESSIONID cookie" in {
    val casParams = CasParams("/secured-service", "foo", "bar")
    val casMock = new CasMock(params = casParams, sessionGen = (_) => "session1")
    val mock = Client(
      open = Service.lift((req: Request) => req match {
        case req@GET -> Root / "secured" if checkSession(req, "session1") =>
          Ok("OK").map(DisposableResponse(_, Task.now[Unit] {}))
        case _ =>
          NotFound().map(DisposableResponse(_, Task.now[Unit] {}))
      }),
      shutdown = Task.now[Unit] {}
    )
    val casClient = new CasClient(virkailijaUri, casMock.client)
    val client = CasAuthenticatingMiddleware(casClient, casParams)(mock)
    val requestUri = resolve(virkailijaUri, uri("/secured"))
    client.prepare(requestUri).run.status should be(Status.Ok)
  }

  it should "refresh the expired CAS session and repeat the request with the new session" in {
    val casParams = CasParams("/secured-service", "foo", "bar")
    val casMock = new CasMock(params = casParams)
    val mock = Client(
      shutdown = Task.now[Unit] {},
      open = Service.lift((req: Request) => req match {
        case req@GET -> Root / "secured" if checkSession(req, CasServer.initialTgt.toString) =>
          Found(resolve(virkailijaUri, uri("/cas/login"))).map(DisposableResponse(_, Task.now[Unit] {}))
        case req@GET -> Root / "secured" if checkSession(req, (CasServer.initialTgt + 1).toString) =>
          Ok("Ok").map(DisposableResponse(_, Task.now[Unit] {}))
      })
    )

    val casClient = new CasClient(virkailijaUri, casMock.client)
    val client = CasAuthenticatingMiddleware(casClient, CasParams("/secured-service", "foo", "bar"))(mock)

    val requestUri = resolve(virkailijaUri, uri("/secured"))
    client.prepare(requestUri).run.status should be(Status.Ok)
  }


  it should "retain the headers from the given request" in {
    val casParams = CasParams("/secured-service", "foo", "bar")
    val casMock = new CasMock(params = casParams)
    val reqheaders = Headers(`Content-Type`(MediaType.`application/excel`), headers.Cookie(Cookie("foo", "bar"), Cookie("fuu", "bur")))
    val mock = Client(
      shutdown = Task.now[Unit] {},
      open = Service.lift((req: Request) => req match {
        case req@GET -> Root / "secured" if checkSession(req, CasServer.initialTgt.toString) && !reqheaders.exists(header => !req.headers.toList.contains(header)) =>
          Ok("Ok").map(DisposableResponse(_, Task.now[Unit] {}))
      })
    )

    val casClient = new CasClient(virkailijaUri, casMock.client)
    val client = CasAuthenticatingMiddleware(casClient, CasParams("/secured-service", "foo", "bar"))(mock)

    val requestUri = resolve(virkailijaUri, uri("/secured"))

    val req = Request(
      Method.GET,
      requestUri,
      headers = reqheaders
    )

    client(req).run.status should be(Status.Ok)
  }


  def getSession(req: Request) = req.headers.collect {
    case headers.Cookie(cookies: headers.Cookie) => cookies.values.list
  }.flatten.collectFirst {
    case cookie if cookie.name == "JSESSIONID" => cookie.content

  }

  def checkSession(req: Request, sessionid: String) = req.headers.collectFirst {
    case headers.Cookie(cookies: headers.Cookie) if cookies.values.list.exists((cookie) => cookie.name == "JSESSIONID" && cookie.content == s"foobar-$sessionid") => true
  }.isDefined

}

