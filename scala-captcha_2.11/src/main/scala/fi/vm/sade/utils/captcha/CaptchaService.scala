package fi.vm.sade.utils.captcha

import fi.vm.sade.utils.http.DefaultHttpClient
import fi.vm.sade.utils.slf4j.Logging
import org.json4s.jackson.JsonMethods._

trait CaptchaServiceComponent {

  def captchaService(): CaptchaService

  trait CaptchaService {
    def checkCaptcha(captcha: String): Boolean
  }

  class RemoteCaptchaService(settings: CaptchaServiceSettings) extends CaptchaService with JsonFormats with Logging {
    def checkCaptcha(captcha: String): Boolean = {
      if (settings.recaptchaSecret.isEmpty) {
        true
      }
      else {
        val recaptchaCheckRequest = DefaultHttpClient.httpGet(settings.recaptchaUrl + "?secret=" + settings.recaptchaSecret + "&response=" + captcha)(settings.recaptchaCallerId)

        recaptchaCheckRequest.responseWithHeaders() match {
          case (200, _, result) => {
            val success = (parse(result) \ "success").extract[Boolean]
            if (!success) logger.warn(s"Captcha check failed with response: $result")
            success
          }
          case (errorcode, _, result) => {
            throw new IllegalStateException(s"Captcha check request failed with responsecode $errorcode and response $result")
          }
        }
      }
    }
  }
}
