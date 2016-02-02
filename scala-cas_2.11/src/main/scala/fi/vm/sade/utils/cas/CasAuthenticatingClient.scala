package fi.vm.sade.utils.cas

import org.http4s._
import org.http4s.client.Client
import org.http4s.headers.Location
import scalaz.concurrent.Task
import scalaz.stream._
import CasClient._

/**
 *  HTTP client implementation that handles CAS authentication automatically
 */
class CasAuthenticatingClient(casClient: CasClient, casParams: CasParams, serviceClient: Client) extends Client {
  override def prepare(req: Request): Task[Response] = {
    def requestProcess(req: Request): Process[Task, Response] = Process(req).toSource zip (sessions) through requestChannel flatMap { case resp if sessionExpired(resp) => sessionRefreshProcess.drain ++ requestProcess(req)
    case resp => Process(resp).toSource
    }
    requestProcess(req).runLast.map(_.getOrElse(throw new Exception("FAILURE!!!!")))
  }

  override def shutdown(): Task[Unit] = serviceClient.shutdown()

  private val paramSource: Process[Task, CasParams] = Process(casParams).toSource
  private val sessions = paramSource through casClient.casSessionChannel
  private val sessionRefreshProcess = paramSource through casClient.sessionRefreshChannel
  private val requestChannel = channel.lift[Task, Request, Response]((req: Request) => serviceClient.prepare(req)).contramap[(Request, JSessionId)] {
    case (req: Request, session: JSessionId) => req.putHeaders(headers.Cookie(Cookie("JSESSIONID", session)))
  }

  private def sessionExpired(resp: Response): Boolean =
    resp.status.code == Status.Found.code && resp.headers.get(Location).exists(_.value.contains("/cas/login"))
}