package fi.vm.sade.utils.http

import scalaj.http.{Http, HttpOptions}

trait HttpClient {
  def httpGet(url: String) : HttpRequest
  def httpGet(url: String, options: HttpOptions.HttpOption*) : HttpRequest
  def httpPost(url: String, data: Option[String]) : HttpRequest
  def httpPost(url: String, data: Option[String], options: HttpOptions.HttpOption*) : HttpRequest
}

object DefaultHttpClient extends HttpClient {
  val defaultOptions: Seq[HttpOptions.HttpOption] = Seq(HttpOptions.connTimeout(10000), HttpOptions.readTimeout(60000))

  def httpGet(url: String) : HttpRequest = {
    httpGet(url, defaultOptions: _*)
  }

  def httpGet(url: String, options: HttpOptions.HttpOption*) : HttpRequest = {
    new DefaultHttpRequest(changeOptions(Http.get(url), options: _*))
  }

  def httpPost(url: String, data: Option[String]) : HttpRequest = {
    httpPost(url, data, defaultOptions: _*)
  }

  def httpPost(url: String, data: Option[String], options: HttpOptions.HttpOption*) : HttpRequest = {
    new DefaultHttpRequest(changeOptions(data match {
      case None => Http.post(url)
      case Some(data) => Http.postData(url, data)
    }, options: _*))
  }

  private def changeOptions(request: Http.Request, options: HttpOptions.HttpOption*): Http.Request = {
    request.options(options: _*)
  }
}
