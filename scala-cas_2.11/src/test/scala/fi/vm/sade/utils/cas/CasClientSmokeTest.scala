package fi.vm.sade.utils.cas

import java.net.ConnectException

import fi.vm.sade.utils.cas.CasClient.SessionCookie
import fi.vm.sade.tcp.PortChecker
import org.http4s.Uri
import org.http4s.client.blaze
import org.scalatest.{FreeSpec, Matchers}

import scala.util.{Failure, Try}

class CasClientSmokeTest extends FreeSpec with Matchers {

  val params: CasParams = CasParams("http://service", "suffix", "u", "pw")

  "CasClient fetch session should fail with correct (inner) exception instead of some functional problem" in {
    val virkailijaUrl = "http://localhost:" + PortChecker.findFreeLocalPort()
    val casClient = new CasClient(virkailijaUrl, blaze.defaultClient, "my-caller-id")

    val result: Try[SessionCookie] = Try(casClient.fetchCasSession(params).unsafePerformSync)

    (result match {
      case Failure(e) if e.isInstanceOf[ConnectException] && e.getMessage.equals("Connection refused") =>
        true
      case _ =>
        false
    }) shouldBe true
  }

  "Http4s Uri resolve function is broken and cannot be used until upgrade" in {
    // http4s version used by this lib doesn't support appending segments to path which is why this test is added here
    // to remind the fortunate upgrader that if you do update the http4s, please check all uses of `resolve` function
    // and also calls to `withPath` to ensure correct functionality across the entire CAS client library
    //
    // Also note that this behavior, while compatible with RFC, does not match what eg. `java.net.URI#resolve` or
    // `java.nio.Path#resolve` does
    (Uri.uri("http://localhost/foo").resolve(Uri.uri("/bar"))) shouldBe Uri.uri("http://localhost/foo")  // this is broken
    (Uri.uri("http://localhost/foo").withPath("/bar")) shouldBe Uri.uri("http://localhost/bar")  // this is fine
  }
}
