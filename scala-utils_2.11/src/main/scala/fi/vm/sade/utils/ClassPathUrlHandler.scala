package fi.vm.sade.utils

import java.io.IOException
import java.net.{URL, URLConnection, URLStreamHandler}

import fi.vm.sade.utils.config.ConfigTemplateProcessor.getClass

class ClassPathUrlHandler(val classLoader: ClassLoader = getClass.getClassLoader) extends URLStreamHandler {
  @throws[IOException]
  protected def openConnection(u: URL): URLConnection = {
    val resourceUrl = classLoader.getResource(u.getPath)
    resourceUrl.openConnection
  }
}
