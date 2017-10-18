package fi.vm.sade.utils.cas

import org.http4s.{Uri, _}
import org.http4s.client.blaze
import org.scalatest.{Tag, FreeSpec, Matchers}

import scalaz.{-\/, \/-}
class CasAuthenticatingClientIntegrationTest extends FreeSpec with Matchers {
  private def uriFromString(uri: String): Uri = {
    Uri.fromString(uri) match {
      case \/-(result) => result
      case -\/(failure) =>
        throw new IllegalArgumentException("Cannot create URI: " + uri + ": " + failure)
    }
  }
  private def requiredEnv(name: String) = scala.util.Properties.envOrNone(name).getOrElse(throw new IllegalStateException("Environment property " + name + " missing"))

  "CasAuthenticatingClient integration test" taggedAs(IntegrationTestTag) in {
    val virkailijaUrl: String = requiredEnv("VIRKAILIJA_ROOT")
    val virkailijaUser: String = requiredEnv("VIRKAILIJA_USER")
    val virkailijaPassword: String = requiredEnv("VIRKAILIJA_PASSWORD")

    val casClient = new CasClient(virkailijaUrl, blaze.defaultClient)
    val casAuthenticatingClient = CasAuthenticatingClient(
      casClient,
      CasParams("/authentication-service", virkailijaUser, virkailijaPassword),
      blaze.defaultClient,
      Some("koski"),
      "JSESSIONID"
    )
    val request = Request(uri = uriFromString(virkailijaUrl + "/authentication-service/resources/omattiedot"))
    val result = casAuthenticatingClient.fetch(request) { response =>
      response.status match {
        case Status.Ok => response.as[String]
        case other => throw new RuntimeException("Response code " + other)
      }

    }.run
    println(result)
  }
}

object IntegrationTestTag extends Tag("integrationtest")
