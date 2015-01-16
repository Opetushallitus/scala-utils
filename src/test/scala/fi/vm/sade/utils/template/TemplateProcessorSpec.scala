package fi.vm.sade.utils.template

import org.specs2.mutable.Specification

class TemplateProcessorSpec extends Specification {
  "TemplateProcessor" should {
    "process YAML attribute file" in {
      TemplateProcessor.processMustacheWithYamlAttributes("src/test/resources/template/test.properties.template", "src/test/resources/template/test.properties.yml") === "value=abc\n"
    }
    "process simple map" in {
      TemplateProcessor.processTemplate("src/test/resources/template/list.mustache", Map(
        "repo" -> List(
          Map("name" -> "resque"),
          Map("name" -> "hub"),
          Map("name" -> "rip")
        )
      )) === "List:\n<b>resque</b>\n<b>hub</b>\n<b>rip</b>\n";
    }
  }
}

