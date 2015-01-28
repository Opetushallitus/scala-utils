package fi.vm.sade.utils.http

import scalaj.http.{Http, HttpOptions}

trait HttpClient {
  def httpGet(url: String) : HttpRequest
  def httpGet(url: String, options: HttpOptions.HttpOption*) : HttpRequest
  def httpPost(url: String, data: Option[String]) : HttpRequest
  def httpPost(url: String, data: Option[String], options: HttpOptions.HttpOption*) : HttpRequest
  def httpPut(url: String) : HttpRequest
  def httpPut(url: String, options: HttpOptions.HttpOption*) : HttpRequest
}

object DefaultHttpClient extends HttpClient {
  val defaultOptions: Seq[HttpOptions.HttpOption] = Seq(HttpOptions.connTimeout(10000), HttpOptions.readTimeout(60000))

  def httpGet(url: String) : HttpRequest = {
    httpGet(url, defaultOptions: _*)
  }

  def httpGet(url: String, options: HttpOptions.HttpOption*) : HttpRequest = {
    new DefaultHttpRequest(Http(url).method("GET").options(options))
  }

  def httpPost(url: String, data: Option[String]) : HttpRequest = {
    httpPost(url, data, defaultOptions: _*)
  }

  def httpPost(url: String, data: Option[String], options: HttpOptions.HttpOption*) : HttpRequest = {
    val postRequest = Http(url).method("POST").options(options)
    data match {
      case None => new DefaultHttpRequest(postRequest)
      case Some(data) => new DefaultHttpRequest(postRequest.postData(data))
    }
  }

  def httpPut(url: String) : HttpRequest = {
    httpPut(url, defaultOptions: _*)
  }

  def httpPut(url: String, options: HttpOptions.HttpOption*) : HttpRequest = {
    new DefaultHttpRequest(Http(url).method("PUT").options(options))
  }

}
