package fi.vm.sade.utils.config

import java.io.StringReader
import java.util.Properties

import com.typesafe.config.ConfigFactory
import fi.vm.sade.utils.template.TemplateProcessor

object ConfigTemplateProcessor {
  def createSettings[T <: ApplicationSettings](projectName: String, attributesFile: String)(implicit applicationSettingsParser: ApplicationSettingsParser[T]): T = {
    val templateFile: String = "src/main/resources/oph-configuration/" + projectName + ".properties.template"
    val templatedData = TemplateProcessor.processMustacheWithYamlAttributes(templateFile, attributesFile) + "\nmongodb.ensureIndex=false" // <- to make work with embedded mongo
    val properties = new Properties()
    properties.load(new StringReader(templatedData))
    applicationSettingsParser.parse(ConfigFactory.load(ConfigFactory.parseProperties(properties)))
  }
}
