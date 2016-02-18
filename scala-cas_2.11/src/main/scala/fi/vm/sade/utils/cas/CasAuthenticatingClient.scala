package fi.vm.sade.utils.cas

import fi.vm.sade.utils.cas.CasClient._
import org.http4s._
import org.http4s.client.{Client, DisposableResponse}
import org.http4s.headers.Location

import scalaz.concurrent.Task


/**
 *  HTTP client implementation that handles CAS authentication automatically. Sessions are maintained by keeping
 *  a central cache of JSESSIONids per service url. If a JSESSIONID is not found for requested service, it is obtained using
 *  CasClient. Stale sessions are detected and refreshed automatically.
 */
object CasAuthenticatingClient extends Logging {
  def apply(casClient: CasClient, casParams: CasParams, serviceClient: Client): Client = new CasAuthenticatingClient(casClient, casParams, serviceClient).httpClient
}

class CasAuthenticatingClient(casClient: CasClient, casParams: CasParams, serviceClient: Client) extends Logging {
  lazy val httpClient = Client(
    open = Service.lift(open _),
    shutdown = serviceClient.shutdown
  )

  private var sessions: collection.mutable.Map[CasParams, JSessionId] = collection.mutable.Map.empty

  private def open(req: Request): Task[DisposableResponse] = {
    openWithCasSession(getCasSession(casParams), req).flatMap {
      case resp if sessionExpired(resp.response) =>
        logger.debug("Session for " + casParams + " expired")
        openWithCasSession(refreshSession(casParams), req)
      case resp =>
        Task.now(resp)
    }
  }

  private def openWithCasSession(sessionIdTask: Task[JSessionId], request: Request): Task[DisposableResponse] = {
    sessionIdTask.flatMap { jsessionid =>
      serviceClient.open(request.putHeaders(headers.Cookie(Cookie("JSESSIONID", jsessionid))))
    }
  }

  private def sessionExpired(resp: Response): Boolean = {
    resp.status.code == Status.Found.code && resp.headers.get(Location).exists(_.value.contains("/cas/login"))
  }

  private def getCasSession(params: CasParams): Task[JSessionId] = {
    synchronized(sessions.get(params)) match {
      case None =>
        logger.debug("No existing jsessionid found for " + params + ", creating new")
        refreshSession(params)
      case Some(session) =>
        Task.now(session)
    }
  }

  private def refreshSession(params: CasParams): Task[JSessionId] = {
    casClient.fetchCasSession(params).map { session =>
      logger.debug("Storing new jsessionid for " + params)
      synchronized(sessions.put(params, session))
      session
    }
  }
}