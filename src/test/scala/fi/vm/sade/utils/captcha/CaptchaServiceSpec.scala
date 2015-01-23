package fi.vm.sade.utils.captcha

import com.typesafe.config.{ConfigException, ConfigFactory, Config}
import org.specs2.mutable.Specification
import scala.collection.JavaConversions._

class CaptchaServiceSpecConfig(config: Config) extends CaptchaServiceComponent {
  val captchaService = new RemoteCaptchaService(new CaptchaServiceSettings(config))
}

class CaptchaServiceSpec extends Specification {

  "CaptchaService without properties set" should {

    "should fail" in {
      new CaptchaServiceSpecConfig(ConfigFactory.empty()) must throwA(new ConfigException.Missing("recaptcha"))
    }
  }

  "CaptchaService with no secret" should {
    val config: Map[String, String] = Map("recaptcha.url" -> "http:/cc.cd", "recaptcha.secret" -> "")
    val service =  new CaptchaServiceSpecConfig(ConfigFactory.parseMap(config)).captchaService

    "should accept any captcha" in {
      service.checkCaptcha("") must_== true
      service.checkCaptcha("xyz") must_== true
    }
  }

  "CaptchaService with wrong service and wrong secret" should {
    val config: Map[String, String] = Map("recaptcha.url" -> "http://x.x", "recaptcha.secret" -> "xyz")
    val service =  new CaptchaServiceSpecConfig(ConfigFactory.parseMap(config)).captchaService
    val expectedError = new IllegalStateException("Captcha check request failed with responsecode 500 and response java.net.UnknownHostException: x.x")

    "should throw an exception with any captcha" in {
      service.checkCaptcha("") must throwA(expectedError)
        service.checkCaptcha("xyz") must throwA(expectedError)
    }
  }

  "CaptchaService with real service but wrong secret" should {
    val config: Map[String, String] = Map("recaptcha.url" -> "https://www.google.com/recaptcha/api/siteverify", "recaptcha.secret" -> "xyz")
    val service =  new CaptchaServiceSpecConfig(ConfigFactory.parseMap(config)).captchaService

    "should fail with any captcha" in {
      service.checkCaptcha("") must_== false
      service.checkCaptcha("xyz") must_== false
    }
  }
}

