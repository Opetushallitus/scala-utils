package fi.vm.sade.utils.http

import java.net.URL

import fi.vm.sade.utils.slf4j.Logging

import scala.collection.immutable.HashMap
import scalaj.http.Http.Request
import scalaj.http.{Http, HttpException}

trait HttpRequest{
  def responseWithHeaders(): (Int, Map[String, List[String]], String)
  def response(): Option[String]
  def param(key: String, value: String): HttpRequest
  def header(key: String, value: String): HttpRequest
  def getUrl: URL
}

class DefaultHttpRequest(private val request: Request) extends HttpRequest with Logging {
  def param(key: String, value: String) = {
    new DefaultHttpRequest(request.param(key, value))
  }

  def header(key: String, value: String) = {
    new DefaultHttpRequest(request.header(key, value))
  }

  def responseWithHeaders(): (Int, Map[String, List[String]], String) = {
    try {
      request.asHeadersAndParse(Http.readString)
    } catch {
      case e: HttpException => {
        if(e.code != 404) logUnexpectedError(e)
        (e.code, HashMap(), e.body)
      }
      case t: Throwable => {
        logUnexpectedError(t)
        (500, HashMap(), t.toString)
      }
    }
  }

  def response(): Option[String] = {
    try {
      Some(request.asString)
    } catch {
      case e: HttpException => {
        if(e.code != 404) logUnexpectedError(e)
        None
      }
      case t: Throwable => {
        logUnexpectedError(t)
        None
      }
    }
  }

  private def logUnexpectedError(t: Throwable) {
    logger.error("Unexpected error from " + request.method + " to " + request.url + " : " + t, t)
  }

  def getUrl() = request.getUrl
}
