package fi.vm.sade.scalaproperties

import scala.collection.Map

/**
 * Extends OphProperties with scala types
 */
class OphProperties extends fi.vm.sade.properties.OphProperties {
  private val excludeCCFields = List("$outer")
  private def caseClassToMap(cc: Product) = {
    val declaredFields = cc.getClass.getDeclaredFields.toList.filter( f => !excludeCCFields.contains(f.getName))
    (Map[String, Any]() /: declaredFields) {(a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(cc))
    }
  }

  private def removeOption(map: Map[String, Any]) = {
    for( (k,v) <- map if v != None)
      yield (k, v match {
        case Some(option)  => option
        case _ => v
      } )
  }

  private def toJavaMap(map: Map[String, Any]) = collection.JavaConversions.mapAsJavaMap(removeOption(map))

  override def convertParams(params: AnyRef*): Array[AnyRef] = {
    params.map {
      case map: Map[String, Any] =>
        toJavaMap(map.asInstanceOf[Map[String, Any]])
      case cc: Product =>
        toJavaMap(caseClassToMap(cc))
      case o =>
        o
    }.toArray
  }
}