package fi.vm.sade.utils.config

import java.io.File

import com.typesafe.config.{ConfigFactory, ConfigValueFactory, Config}
import fi.vm.sade.utils.slf4j.Logging

object ApplicationSettingsLoader extends Logging {
  def loadSettings[T <: ApplicationSettings](fileLocation: String)(implicit parser: ApplicationSettingsParser[T]): T = {
    val configFile = new File(fileLocation)
    if (configFile.exists()) {
      logger.info("Using configuration file " + configFile)
      parser.parse(ConfigFactory.load(ConfigFactory.parseFile(configFile)))
    } else {
      throw new RuntimeException("Configuration file not found: " + fileLocation)
    }
  }
}

abstract class ApplicationSettings(config: Config) {

  import scala.collection.JavaConversions._

  def toProperties = {
    val keys = config.entrySet().toList.map(_.getKey)
    keys.map { key =>
      (key, config.getString(key))
    }.toMap
  }

  def withOverride[T <: ApplicationSettings](keyValuePair : (String, String))(implicit parser: ApplicationSettingsParser[T]): T = {
    parser.parse(config.withValue(keyValuePair._1, ConfigValueFactory.fromAnyRef(keyValuePair._2)))
  }

  protected def getMongoConfig(config: Config) = {
    MongoConfig(
      config.getString("uri"),
      config.getString("dbname")
    )
  }
}

trait ApplicationSettingsParser[T <: ApplicationSettings] {

  def parse(config: Config): T

}

case class MongoConfig(url: String, dbname: String)