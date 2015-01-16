package fi.vm.sade.utils.template

import java.io.{File, FileInputStream}
import java.util.HashMap

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.`type`.MapType
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import org.fusesource.scalate.TemplateEngine
import org.fusesource.scalate.support.FileTemplateSource

import scala.collection.JavaConverters._

object TemplateProcessor {

  def processMustacheWithYamlAttributes(templatePath: String, yamlFile: String): String = {
    val mapper: ObjectMapper = new ObjectMapper(new YAMLFactory())
    val mapType: MapType = mapper.getTypeFactory.constructMapType(classOf[HashMap[String, String]], classOf[String], classOf[String])
    val rawValue = mapper.readValue(new FileInputStream(yamlFile), mapType).asInstanceOf[HashMap[String, String]]
    val attributes: Map[String, Any] = rawValue.asScala.toMap.asInstanceOf[Map[String, Any]]

    processMustache(templatePath, attributes)
  }

  def processMustache(templatePath: String, attributes: Map[String, Any]): String = {
    val engine = new TemplateEngine
    engine.layout(new FileTemplateSource(new File(templatePath), "template.mustache"), attributes)
  }
}
