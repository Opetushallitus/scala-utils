package fi.vm.sade.utils.cas

import fi.vm.sade.utils.cas.CasClient._
import scala.collection.mutable.ListBuffer

import org.http4s.{headers, _}
import org.http4s.client.{Client, DisposableResponse}
import org.http4s.headers.Location

import scalaz.concurrent.Task


/**
 *  HTTP client implementation that handles CAS authentication automatically. Sessions are maintained by keeping
 *  a central cache of JSESSIONids per service url. If a JSESSIONID is not found for requested service, it is obtained using
 *  CasClient. Stale sessions are detected and refreshed automatically.
 */
object CasAuthenticatingClient extends Logging {
  def apply(casClient: CasClient, casParams: CasParams, serviceClient: Client, clientSubSystemCode: String = null): Client = {
    new CasAuthenticatingClient(casClient, casParams, serviceClient, clientSubSystemCode).httpClient
  }
}

class CasAuthenticatingClient(casClient: CasClient, casParams: CasParams, serviceClient: Client, clientSubSystemCode: String) extends Logging {
  lazy val httpClient = Client(
    open = Service.lift(open _),
    shutdown = serviceClient.shutdown
  )

  private var sessions: collection.mutable.Map[CasParams, JSessionId] = collection.mutable.Map.empty

  private def open(req: Request): Task[DisposableResponse] = {
    openWithCasSession(getCasSession(casParams), req).flatMap {
      case resp if sessionExpired(resp.response) =>
        logger.debug("Session for " + casParams + " expired")
        resp.dispose.flatMap(_ => openWithCasSession(refreshSession(casParams), req))
      case resp =>
        Task.now(resp)
    }
  }

  private def addHeaders(req: Request, session: JSessionId): Request = {
    val csrf = "CasAuthenticatingClient"
    var list: ListBuffer[Header] = ListBuffer(headers.Cookie(Cookie("JSESSIONID", session), Cookie("CSRF", csrf)), Header("CSRF", csrf))
    if (clientSubSystemCode != null) {
      list += Header("clientSubSystemCode", clientSubSystemCode)
    }
    req.putHeaders(list: _*)
  }

  private def openWithCasSession(sessionIdTask: Task[JSessionId], request: Request): Task[DisposableResponse] = {
    sessionIdTask.flatMap { jsessionid =>
      val requestWithHeaders = addHeaders(request, jsessionid)
      serviceClient.open(requestWithHeaders)
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
