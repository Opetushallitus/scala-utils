package fi.vm.sade.utils.cas

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fi.vm.sade.utils.cas.CasAuthenticatingClient.DefaultSessionCookieName
import fi.vm.sade.utils.cas.CasClient.SessionCookie
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.ConnectionFailure
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.global
import scala.util.{Failure, Random, Try}

import java.io.IOException
import java.net.Socket


class CasClientSmokeTest extends AnyFreeSpec with Matchers {
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  val params: CasParams = CasParams("http://service", "suffix", "u", "pw")

  "CasClient fetch session should fail with correct (inner) exception instead of some functional problem" in {
    val virkailijaUrl = "http://localhost:" + findFreeLocalPort.toString
    val blazeClient = BlazeClientBuilder[IO](global).allocated.map(_._1).unsafeRunSync()
    val casClient = new CasClient(virkailijaUrl, blazeClient, "my-caller-id")

    val result: Try[SessionCookie] = Try(casClient.fetchCasSession(params, DefaultSessionCookieName).unsafeRunSync())

    result match {
      case Failure(e) =>
        e shouldBe a[ConnectionFailure]
        e.getMessage should startWith("Error connecting to")
      case _ => fail("Should not succeed")
    }
  }

  def isFreeLocalPort(port: Int): Boolean = {
    try {
      val socket = new Socket("127.0.0.1", port)
      socket.close()
      false
    } catch {
      case e:IOException => true
    }
  }

  def findFreeLocalPort: Int = {
    val range = 1024 to 60000
    val port = ((range(new Random().nextInt(range length))))
    if (isFreeLocalPort(port)) {
      port
    } else {
      findFreeLocalPort
    }
  }
}
