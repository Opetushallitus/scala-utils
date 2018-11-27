package fi.vm.sade.utils.kayttooikeus

import scala.util.Try
import scala.util.control.NonFatal
import org.json4s.jackson.JsonMethods.parse
import fi.vm.sade.utils.http.DefaultHttpClient
import scalaj.http.HttpOptions


class KayttooikeusUserDetailsService() {
  import org.json4s._
  implicit val formats = DefaultFormats

  def getUserByUsername(username: String, callerId: String, ophProperties: fi.vm.sade.properties.OphProperties): Either[Throwable, KayttooikeusUserDetails] = {
    getUserByUsername(username, callerId, ophProperties.getProperty("kayttooikeus-service.userDetails.byUsername", username))
  }

  def getUserByUsername(username: String, callerId: String, userDetailsUrl: String): Either[Throwable, KayttooikeusUserDetails] = {

    fetch(userDetailsUrl, callerId) { response =>
      // response username field contains actually oid because of historical ldap reasons
      val koDto = parse(response).extract[KayttooikeusUserResp]
      KayttooikeusUserDetails(koDto.authorities.map(x => x.authority.replace("ROLE_","")).toList, koDto.username)
    }.left.map {
      case e: IllegalArgumentException => new IllegalArgumentException(s"User not found with username: $username", e)
      case e: Exception => new RuntimeException(s"Failed to get username $username details", e)
    }
  }

  private def fetch[T](url: String, callerId: String)(parse: (String => T)): Either[Throwable, T] = {
    Try(DefaultHttpClient.httpGet(
      url,
      HttpOptions.connTimeout(5000),
      HttpOptions.readTimeout(10000)
    ).header("Caller-id", callerId)
      .responseWithHeaders match {
      case (200, _, resultString) =>
        Try(Right(parse(resultString))).recover {
          case NonFatal(e) => Left(new IllegalStateException(s"Parsing result $resultString of GET $url failed", e))
        }.get
      case (404, _, resultString) =>
        Left(new IllegalArgumentException(s"User not found"))
      case (responseCode, _, resultString) =>
        Left(new RuntimeException(s"GET $url failed with status $responseCode: $resultString"))
    }).recover {
      case NonFatal(e) => Left(new RuntimeException(s"GET $url failed", e))
    }.get
  }

}

