package fi.vm.sade.scalaproperties

import java.util.Properties
import org.scalatest.flatspec.AnyFlatSpec

class OphPropertiesSpec extends AnyFlatSpec {

  "Scala conversions" should "work with OphProperties" in {
    case class Pow(param: String, b: Option[String] = None)
    val ctx: OphProperties = new OphProperties()
    val props = new Properties()
    ctx.ophProperties = props
    props.setProperty("a.b", "1")
    props.setProperty("b.b", "$1 $param")
    assert(ctx.url("a.b") == "1")
    assert(ctx.url("b.b", "c", Pow("2")) == "c 2")
    assert(ctx.url("a.b", Pow("2", Some("POW!"))) == "1?param=2&b=POW!")
    assert(ctx.url("b.b", Pow("2", Some("POW!"))) == "$1 2?b=POW!")
    assert(ctx.url("b.b", Map("a" -> List("a", "b"))) == "$1 $param?a=a&a=b" )
  }
}