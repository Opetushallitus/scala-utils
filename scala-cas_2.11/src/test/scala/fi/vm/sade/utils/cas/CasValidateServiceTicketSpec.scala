package fi.vm.sade.utils.cas

import java.lang.Process

import fi.vm.sade.utils.cas.CasClient.Username
import org.http4s.{Response, Uri}
import org.http4s.dsl._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.xml.Node
import org.scalatest.{Matchers, FlatSpec}

@RunWith(classOf[JUnitRunner])
class CasValidateServiceTicketSpec extends FlatSpec with Matchers {

  behavior of "ServiceTicketResponseXmlDecoder"


  it should "validate authentication success and return correct username" in {
    val cas: Node = <cas:serviceResponse xmlns:cas="http://www.yale.edu/tp/cas">
      <cas:authenticationSuccess>
        <cas:user>robotti</cas:user>
      </cas:authenticationSuccess>
    </cas:serviceResponse>

    val user: Username = ServiceTicketResponseXmlDecoder.decodeUsername(Response().withBody(cas.toString()).run).run

    user should equal ("robotti")
  }

}
