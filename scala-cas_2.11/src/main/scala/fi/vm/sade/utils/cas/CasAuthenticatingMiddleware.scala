package fi.vm.sade.utils.cas

import org.http4s.{headers, _}
import org.http4s.client.{Client, DisposableResponse}
import org.http4s.headers.Location

import scalaz.concurrent.Task
import scalaz.stream._
import CasClient._

import scala.collection.mutable.ListBuffer

/**
 *  HTTP client middleware that handles CAS authentication
 */
object CasAuthenticatingMiddleware {
  def apply(casClient: CasClient, casParams: CasParams, clientSubSystemCode: String = null)(client: Client): Client = {
    val paramSource = Process(casParams).toSource
    val sessions = paramSource through casClient.casSessionChannel
    val sessionRefreshProcess = paramSource through casClient.sessionRefreshChannel
    val requestChannel: Channel[Task, (Request, JSessionId), DisposableResponse] =
      channel.lift[Task, Request, DisposableResponse](client.open(_)).contramap[(Request, JSessionId)] {
        case (req: Request, session: JSessionId) => addHeaders(req, session, clientSubSystemCode)
      }
    def foo(req: Request): Task[DisposableResponse] = {
      def requestProcess(req: Request): Process[Task, DisposableResponse] = {
        (Process(req).toSource zip sessions through requestChannel).flatMap {
          case resp if sessionExpired(resp) => sessionRefreshProcess.drain ++ requestProcess(req)
          case resp => Process(resp).toSource
        }
      }
      requestProcess(req).runLast.map(_.getOrElse(throw new Exception("FAILURE!!!!")))
    }
    client.copy(open = Service.lift(foo), shutdown = client.shutdown)
  }

  private def sessionExpired(resp: DisposableResponse): Boolean =
    resp.response.status.code == Status.Found.code && resp.response.headers.get(Location).exists(_.value.contains("/cas/login"))

  private def addHeaders(req: Request, session: JSessionId, clientSubSystemCode: String): Request = {
    val csrf = "CasAuthenticatingClient"
    var list: ListBuffer[Header] = ListBuffer(headers.Cookie(Cookie("JSESSIONID", session), Cookie("CSRF",csrf)), Header("CSRF", csrf))
    if(clientSubSystemCode != null) {
      list += Header("clientSubSystemCode", clientSubSystemCode)
    }
    req.putHeaders(list:_*)
  }
}
