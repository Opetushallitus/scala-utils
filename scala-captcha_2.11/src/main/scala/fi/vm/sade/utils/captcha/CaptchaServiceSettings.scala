package fi.vm.sade.utils.captcha

import com.typesafe.config.Config
import fi.vm.sade.utils.config.ApplicationSettings

class CaptchaServiceSettings(config: Config) extends ApplicationSettings(config) {
  val recaptchaUrl = config.getString("recaptcha.verify.url")
  val recaptchaSecret = config.getString("recaptcha.secret")
  val recaptchaCallerId = config.getString("recaptcha.caller.id")
}
