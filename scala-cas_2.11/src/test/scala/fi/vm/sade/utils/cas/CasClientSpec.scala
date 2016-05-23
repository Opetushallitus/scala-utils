package fi.vm.sade.utils.cas

import org.http4s.client.{Client, DisposableResponse}
import CasClient._
import org.http4s.dsl._
import org.http4s.headers.{Location, `Set-Cookie`}
import org.http4s.{Request, Response, _}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.stream.async.mutable.Signal

@RunWith(classOf[JUnitRunner])
class CasClientSpec extends FlatSpec with Matchers {
  val virkailijaUri: Uri = uri("https://localhost")
  
  behavior of "CasClient"


  it should "get a TGT ticket" in {
    val casParams = CasParams("/secured-service", "foo", "bar")
    val casMock = CasServer(casParams, tgt = Process("1").repeat)
    val casClient = new CasClient(virkailijaUri, casMock)
    casClient.getTicketGrantingTicket(casParams).run should be (CasServer.tgtUri("TGT-1"))
  }

  it should "get service ticket for TGT" in {
    val casParams = CasParams("/secured-service", "foo", "bar")

    val casMock = CasServer(casParams)

    val casClient = new CasClient(virkailijaUri, casMock)

    val serviceUri = resolve(virkailijaUri, casParams.service.securityUri)

    casClient.getServiceTicket(serviceUri)(CasServer.tgtUri("TGT-1")).run should be ("ST-1")

  }

  it should "get JSESSIONID for Service Ticket" in {
    val casParams = CasParams("/secured-service", "foo", "bar")

    val casMock = CasServer(casParams)

    val casClient = new CasClient(virkailijaUri, casMock)

    val serviceUri = resolve(virkailijaUri, casParams.service.securityUri)


    val securityUri = resolve(serviceUri, uri("j_spring_cas_security_check"))

    casClient.getJSessionId(serviceUri)("ST-1").run should be ("foobar-1")

  }


  it should "give a session id for requested service" in {
    val casParams = CasParams("/sijoittelu-service", "foo", "bar")
    val casMock = new CasMock(params = casParams)
    val casClient = new CasClient(virkailijaUri, casMock.client)

    val params: Process[Task, CasParams] = Process.emit(casParams)

    val session: Process[Task, JSessionId] = params through casClient.casSessionChannel

    session.run.run

    casMock.steps should be (List(
      "created TGT-123",
      "created ST-123",
      "created session foobar-123"
    ))
  }

  it should "give the same session for the same service on subsequential requests" in {
    val casParams = CasParams("/sijoittelu-service", "foo", "bar")
    val casMock = new CasMock(params = casParams)
    val casClient = new CasClient(virkailijaUri, casMock.client)

    val params: Process[Task, CasParams] = Process.emitAll(Seq(casParams, casParams))

    val session: Process[Task, JSessionId] = params through casClient.casSessionChannel

    session.run.run

    casMock.steps should be (List(
      "created TGT-123",
      "created ST-123",
      "created session foobar-123"
    ))
  }

  it should "give a new session when refresh is requested" in {
    val casParams = CasParams("/sijoittelu-service", "foo", "bar")
    val casMock = new CasMock(params = casParams)
    val casClient = new CasClient(virkailijaUri, casMock.client)

    val params: Process[Task, CasParams] = Process.emit(casParams)

    val session: Process[Task, JSessionId] = params through casClient.casSessionChannel

    session.run.run

    //casMock.ticket = "124"

    val refreshedSession: Process[Task, JSessionId] = params through casClient.sessionRefreshChannel

    refreshedSession.run.run

    casMock.steps should be (List(
      "created TGT-123",
      "created ST-123",
      "created session foobar-123",
      "created TGT-124",
      "created ST-124",
      "created session foobar-124"
    ))
  }

}

object CasServer {
  val virkailijaUrl: Uri = uri("https://localhost")
  val initialTgt = 123
  val st: Channel[Task, String, String] = channel.lift[Task, String, String]((tgtId: String) => Task.now(tgtId))

  def failure(message: String) = Task.now[Unit] {}

  def tgtUri(ticket:String) = resolve(virkailijaUrl, Uri(path = s"/cas/v1/tickets/$ticket"))

  def tgtResponse: Channel[Task, String, Response] = channel.lift[Task, String, Response]((ticket) =>
    Created().withHeaders(Location(tgtUri(ticket)))
  )

  def stResponse: Channel[Task, String, Response] = channel.lift[Task, String, Response]((ticket) =>
    Ok(ticket)
  )

  def sesResponse: Channel[Task, (Uri, String), Response] = channel.lift[Task, (Uri, String), Response] { case (redirect, session) =>
    Found(redirect).withHeaders(`Set-Cookie`(Cookie(name = "JSESSIONID", content = session)))
  }

  val tgtPattern = "TGT-(.*)".r

  val stGenerator = process1.lift((stId: String) => s"ST-$stId")

  object ST extends QueryParamDecoderMatcher[String]("ticket")

  val stPattern = "ST-(.*)".r

  val sesGen = process1.lift((stId: String) => s"foobar-$stId")

  def apply(params: CasParams,
            shutdown: Task[Unit] = Task.now[Unit] {},
            preTgt: Sink[Task, String] = sink.lift[Task, String]((_) => Task.now[Unit] {}),
            preSt: Sink[Task, String] = sink.lift[Task, String]((_) => Task.now[Unit] {}),
            preSes:  Sink[Task, String] = sink.lift[Task, String]((_) => Task.now[Unit] {}),
            tgt: Process[Task, String] = Process.supply(initialTgt).map(_.toString),
            session: Channel[Task, String, String] = channel.lift[Task, String, String]((stId: String) => Task.now(stId))): Client =
    Client(
      shutdown = shutdown,
      open = Service.lift((req: Request) => (req match {
        case req@POST -> Root / "cas" / "v1" / "tickets" => req.decode[String] {
          case body if body == s"username=${params.user.username}&password=${params.user.password}" =>
            (tgt.take(1).map((id) => s"TGT-$id") observe preTgt through tgtResponse).runLast.flatMap {
              case Some(r) => Task.now(r)
              case None => InternalServerError("TGT creation failed")
            }
          case _ =>
            failure("invalid login").flatMap((_) =>
              Unauthorized(Challenge("", ""))
            )
        }

        case req@POST -> Root / "cas" / "v1" / "tickets" / tgt => req.decode[UrlForm] {
          case body if body.get("service") == Seq(resolve(virkailijaUrl, params.service.securityUri).toString()) =>
            (Process(tgt).toSource.collect {
              case tgtPattern(ticket) => ticket
            } through st pipe stGenerator observe preSt through stResponse).runLast.flatMap {
              case Some(r) => Task.now(r)
              case None => InternalServerError("ST creation failed")
            }
          case body =>
            println(body.get("service"))
            println(resolve(virkailijaUrl, params.service.securityUri).toString())
            failure("invalid serviceUrl").flatMap((_) =>
              BadRequest()
            )
        }

        case req@GET -> Root / service / "j_spring_cas_security_check" :? ST(ticket) if params.service.securityUri.toString().indexOf(service) > -1 => ticket match {
          case stPattern(stId) =>
            val redirectProcess: Process[Task, Uri] = Process(resolve(virkailijaUrl, Uri(path = "/service"))).toSource
            (redirectProcess zip (Process(stId).toSource through session pipe sesGen observe preSes) through sesResponse).runLast.flatMap {
              case Some(r) => Task.now(r)
              case None => InternalServerError("JSESSIONID creation failed")
            }
          case _ =>
            failure("invalid service ticket").flatMap((_) =>
              Unauthorized(Challenge("", ""))
            )
        }
      }).map(DisposableResponse(_, Task.now[Unit] {})))
    )
}

class CasMock(params: CasParams, sessionGen: (String) => String = identity) {
  val stepState = async.signalOf(List[String]())
  val client = CasServer(
    params,
    shutdown = Task.suspend({ stepState.close }),
    preTgt = setState.contramap((ticket) => s"created $ticket"),
    preSt = setState.contramap((ticket) => s"created $ticket"),
    preSes = setState.contramap((ticket) => s"created session $ticket"),
    session = channel.lift[Task, String, String]((stid) => Task.delay(sessionGen(stid)))
  )

  def steps: List[String] = stepState.get.run

  def setState = stepState.sink.contramap((step:String) => Signal.CompareAndSet[List[String]]{
    case Some(steps) => Some(steps :+ step)
    case None => Some(List(step))
  })
}

