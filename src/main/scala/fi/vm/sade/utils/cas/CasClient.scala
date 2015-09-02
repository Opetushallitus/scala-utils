package fi.vm.sade.utils.cas

import org.http4s.Status.Created
import CasClient._
import org.http4s.Status.ResponseClass.Successful
import org.http4s._
import org.http4s.client._
import org.http4s.dsl._
import org.http4s.headers.{Location, `Set-Cookie`}

import scalaz.concurrent.{Future, Task}
import scalaz.stream.{Channel, async, channel}

object CasClient {
   type JSessionId = String
   type TGTUrl = Uri
   type ST = String

 }

class CasClient(virkailijaLoadBalancerUrl: Uri, client: Client) {
   def this(casServer: String, client: Client) = this(new Task(
     Future.now(
       Uri.fromString(casServer).
         leftMap((fail: ParseFailure) => new IllegalArgumentException(fail.sanitized))
     )).run, client)


   implicit val casUserEncoder = UrlForm.entityEncoder().contramap((user: CasUser) => UrlForm("username" -> user.username, "password" -> user.password))





   private def tgtReq(params: CasParams) = POST(
     resolve(
       virkailijaLoadBalancerUrl,
       uri("/cas/v1/tickets")),
     params.user)


   import TGTDecoder.decodeTgt

   protected[cas] def getTgt(params: CasParams): Task[TGTUrl] =
     client.prepare(tgtReq(params)).flatMap(decodeTgt)



   val stPattern = "(ST-.*)".r

   val stDecoder = EntityDecoder.text.flatMapR[ST]{
     case stPattern(st) => DecodeResult.success(st)
     case nonSt => DecodeResult.failure(ParseFailure("Service Ticket decoding failed", s"response body is of wrong form ($nonSt)"))

   }


   def stReq(tgtUrl: TGTUrl, service:Uri) = POST(
     tgtUrl,
     UrlForm("service" -> service.toString())
   )

   protected[cas] def getServiceTicket(service:Uri)(tgtUrl: TGTUrl) = client.prepAs[ST](stReq(tgtUrl, service))(stDecoder)

   def jSessionReq(serviceTicket:ST, service: Uri) = GET(service.withQueryParam("ticket", serviceTicket))

   protected[cas] def getJSessionId(service:Uri)(serviceTicket:ST): Task[JSessionId] =
     client.prepare(jSessionReq(serviceTicket, service)).flatMap(JSESSIONDecoder.decodeJsession)

   def serviceUrl(params:CasParams) = resolve(virkailijaLoadBalancerUrl, params.service.securityUri)

   protected[cas] def fetchCasSession(params: CasParams) =
     (for (
       tgt <- getTgt(params);
       st <- getServiceTicket(serviceUrl(params))(tgt);
       session <- getJSessionId(serviceUrl(params))(st)
     ) yield session).handle{
       case e@ParseException(pf) =>
         println(pf.details)
         throw e

     }


   private def refreshSession(params: CasParams): Task[JSessionId] = fetchCasSession(params).flatMap((session) => s.compareAndSet {
     case None => Some(Map(params -> session))
     case Some(map) => Some(map.updated(params, session))
   }).map(_.get(params))

   private val s = async.signalOf(Map[CasParams, JSessionId]())

   private def getSession(params: CasParams): Task[Option[JSessionId]] = s.get.map(_.get(params))

   private def fetchSessionFromStore(params: CasParams): Task[JSessionId] = getSession(params).flatMap {
     case Some(session) => Task.now(session)
     case None => refreshSession(params)
   }

   def sessionRefreshChannel: Channel[Task, CasParams, JSessionId] = channel.lift[Task, CasParams, JSessionId] {
     (casParams) => refreshSession(casParams)
   }

   def casSessionChannel: Channel[Task, CasParams, JSessionId] = channel.lift(fetchSessionFromStore)
 }

object TGTDecoder {

   import CasClient.TGTUrl

   val tgtDecoder = EntityDecoder.decodeBy[TGTUrl](MediaRange.`*/*`){
     (msg) =>
       val tgtPattern = "(.*TGT-.*)".r

       msg.headers.get(Location).map(_.value) match {
         case Some(tgtPattern(tgtUrl)) =>
           Uri.fromString(tgtUrl).fold((pf) => DecodeResult.failure(pf), (tgt) => DecodeResult.success(tgt))
         case Some(nontgturl) => DecodeResult.failure(ParseFailure("TGT decoding failed", s"location header has wrong format $nontgturl"))
         case None => DecodeResult.failure(ParseFailure("TGT decoding failed", "No location header"))

       }

   }

   val decodeTgt: (Response) => Task[TGTUrl] = response => DecodeResult.success(response).flatMap[TGTUrl]{
     case Created(resp) => tgtDecoder.decode(resp)
     case resp => DecodeResult.failure(ParseFailure("TGT decoding failed", s"invalid TGT creation status: ${resp.status.code}"))
   }.fold(e => throw new ParseException(e), identity)


 }

object JSESSIONDecoder {

   val jsessionDecoder = EntityDecoder.decodeBy[JSessionId](MediaRange.`*/*`){
     (msg) =>
       msg.headers.collectFirst {
         case `Set-Cookie`(`Set-Cookie`(cookie)) if cookie.name == "JSESSIONID" => DecodeResult.success(cookie.content)
       }.getOrElse(DecodeResult.failure(ParseFailure("Decoding JSESSIONID failed", "no cookie found for JSESSIONID")))

   }

   def decodeJsession(response: Response) = DecodeResult.success(response).flatMap[JSessionId]{
     case Successful(resp) =>  jsessionDecoder.decode(resp)
     case resp =>
       DecodeResult.failure(EntityDecoder.text.decode(resp).fold(
         (_)  => ParseFailure("Decoding JSESSIONID failed", s"service returned non-ok status code ${resp.status.code}"),
         (body) => ParseFailure("Decoding JSESSIONID failed",s"service returned non-ok status code ${resp.status.code}: $body")

       ))
   }.fold(e => throw ParseException(e), identity)
 }