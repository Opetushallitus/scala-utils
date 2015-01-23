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
        val recaptchaCheckRequest = DefaultHttpClient.httpGet(settings.recaptchaUrl + "?secret=" + settings.recaptchaSecret + "&response=" + captcha)

        val response = recaptchaCheckRequest.response()
        val checkResult = for (
          json <- response;
          success <- (parse(json) \ "success").extractOpt[Boolean]
        ) yield success

        checkResult match {
          case Some(success) => {
            if (!success) logger.warn(s"Captcha check failed with response: $response")
            success
          }
          case None => {
            throw new IllegalStateException(s"Captcha check request failed with response: $response")
          }
        }
      }
    }
  }
}
