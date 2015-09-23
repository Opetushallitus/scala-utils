package fi.vm.sade.utils.config

import java.net.URL
import com.typesafe.config.Config
import fi.vm.sade.utils.template.TemplateProcessor
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ConfigTemplateProcessorSpec extends Specification {
  class TestSettings(config: Config) extends ApplicationSettings(config)

  class TestSettingsParser extends ApplicationSettingsParser[TestSettings] {
    override def parse(config: Config) = new TestSettings(config)
  }

  "ConfigTemplateProcessor" should {
    "make application settings from template and vars.yml urls" in {
      val template: URL = getClass.getResource("/template/test.properties.template")
      val vars: URL = getClass.getResource("/template/test.properties.yml")
      implicit val parser = new TestSettingsParser
      val settings: TestSettings = ConfigTemplateProcessor.createSettings(template, vars)
      settings.getStringWithDefault("value", "fail") must_== "abc"
    }
  }
}

