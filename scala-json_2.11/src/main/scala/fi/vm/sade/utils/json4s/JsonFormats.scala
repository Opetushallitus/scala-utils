package fi.vm.sade.utils.json4s

import org.json4s.{Formats, DefaultFormats}
import org.json4s.ext.JodaTimeSerializers

object GenericJsonFormats {
  val genericFormats: Formats =  new DefaultFormats {
    override def dateFormatter = {
      val format = super.dateFormatter
      format.setTimeZone(DefaultFormats.UTC)
      format
    }
  } ++ JodaTimeSerializers.all
}

