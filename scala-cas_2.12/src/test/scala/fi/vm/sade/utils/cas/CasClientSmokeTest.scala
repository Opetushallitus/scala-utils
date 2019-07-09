package fi.vm.sade.utils.cas

import java.net.ConnectException

import fi.vm.sade.utils.cas.CasClient.SessionCookie
import fi.vm.sade.tcp.PortChecker
import org.http4s.client.blaze
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Try}

class CasClientSmokeTest extends FreeSpec with Matchers {

  val params: CasParams = CasParams("http://service", "suffix", "u", "pw")

  "CasClient fetch session should fail with correct (inner) exception instead of some functional problem" in {
    val virkailijaUrl = "http://localhost:" + PortChecker.findFreeLocalPort()
    val casClient = new CasClient(virkailijaUrl, blaze.defaultClient)

    val result: Try[SessionCookie] = Try(casClient.fetchCasSession(params).unsafePerformSync)

    (result match {
      case Failure(e) if e.isInstanceOf[ConnectException] && e.getMessage.equals("Connection refused") =>
        true
      case _ =>
        false
    }) shouldBe true
  }
}
