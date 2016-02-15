package fi.vm.sade.utils.cas

import org.http4s._
import org.http4s.client.{DisposableResponse, Client}
import org.http4s.headers.Location
import scalaz.concurrent.Task
import scalaz.stream._
import CasClient._

/**
 *  HTTP client implementation that handles CAS authentication automatically
 */
object CasAuthenticatingClient {
  def apply(casClient: CasClient, casParams: CasParams, serviceClient: Client): Client = {
    val paramSource: Process[Task, CasParams] = Process(casParams).toSource
    val sessions = paramSource through casClient.casSessionChannel
    val sessionRefreshProcess = paramSource through casClient.sessionRefreshChannel
    val requestChannel = channel.lift[Task, Request, DisposableResponse]((req: Request) => serviceClient.open(req)).contramap[(Request, JSessionId)] {
      case (req: Request, session: JSessionId) => req.putHeaders(headers.Cookie(Cookie("JSESSIONID", session)))
    }

    def sessionExpired(resp: Response): Boolean = resp.status.code == Status.Found.code && resp.headers.get(Location).exists(_.value.contains("/cas/login"))

    def open(req: Request): Task[DisposableResponse] = {
      def requestProcess(req: Request): Process[Task, DisposableResponse] = Process(req).toSource zip (sessions) through requestChannel flatMap {
        case resp if sessionExpired(resp.response) => sessionRefreshProcess.drain ++ requestProcess(req)
        case resp => Process(resp).toSource
      }
      requestProcess(req).runLast.map(_.getOrElse(throw new Exception("FAILURE!!!!")))
    }

    Client(
      open = Service.lift(open _),
      shutdown = serviceClient.shutdown
    )
  }
}