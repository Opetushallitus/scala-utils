package fi.vm.sade.utils.config

import java.io.{File, InputStream, StringReader}
import java.net.URL
import java.util.Properties

import com.typesafe.config.ConfigFactory
import fi.vm.sade.utils.template.TemplateProcessor

object ConfigTemplateProcessor {
  def createSettings[T <: ApplicationSettings](projectName: String, attributesFile: String)(implicit applicationSettingsParser: ApplicationSettingsParser[T]): T = {
    val templateURL: URL = new File("src/main/resources/oph-configuration/" + projectName + ".properties.template").toURI.toURL
    val attributesURL = new File(attributesFile).toURI.toURL

    val templatedData = TemplateProcessor.processMustacheWithYamlAttributes(templateURL, attributesURL) + "\nmongodb.ensureIndex=false" // <- to make work with embedded mongo
    parseTemplatedData(templatedData)
  }

  def createSettings[T <: ApplicationSettings](template: URL, attributes: URL)(implicit applicationSettingsParser: ApplicationSettingsParser[T]): T = {
    val templatedData: String = TemplateProcessor.processMustacheWithYamlAttributes(template, attributes) + "\nmongodb.ensureIndex=false" // <- to make work with embedded mongo
    parseTemplatedData(templatedData)
  }

  def parseTemplatedData[T <: ApplicationSettings](templatedData: String)(implicit applicationSettingsParser: ApplicationSettingsParser[T]): T = {
    val properties = new Properties()
    properties.load(new StringReader(templatedData))
    applicationSettingsParser.parse(ConfigFactory.load(ConfigFactory.parseProperties(properties)))
  }
}
