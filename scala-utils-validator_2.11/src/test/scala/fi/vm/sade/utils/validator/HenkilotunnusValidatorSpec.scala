package fi.vm.sade.utils.validator

import fi.vm.sade.utils.validator.HenkilotunnusValidator._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scalaz.{Success, Failure, ValidationNel}


@RunWith(classOf[JUnitRunner])
class HenkilotunnusValidatorSpec extends FlatSpec with Matchers {

  behavior of "HenkilotunnusValidator"

  implicit def validation2sequence(f: ValidationNel[String, String]): Seq[String] = f match {
    case Success(s) => Seq(s)
    case Failure(fails) => fails.list.toSeq
  }

  it should "validate valid henkilotunnus" in {
    validate("111111-1975").isSuccess should be (true)
  }

  it should "show validation error if henkilotunnus length is not valid" in {
    validate("").toSeq should be (Seq("length is not 11, but 0"))
  }

  it should "not pass validation on insane input" in {
    validate("this is foobar input").isFailure should be (true)
  }

  it should "show validation error if birthday contains invalid characters" in {
    validate("A21111-1975").toSeq should be (Seq("cannot parse birthday part A21111-"))
  }

  it should "show validation error if birthday cannot be parsed" in {
    validate("114111-197B").toSeq should be (Seq("cannot parse birthday 1911-41-11"))
  }

  it should "show validation error if checksum character is not valid" in {
    validate("111111-1976").toSeq should be (Seq("checksum character 6 is invalid"))
  }

  it should "show validation error if separator character is not valid" in {
    validate("111111B1975").toSeq should be (Seq("invalid separator character B"))
  }

  it should "show multiple validation errors if found" in {
    validate("111111B1976").toSeq should be (Seq("invalid separator character B", "checksum character 6 is invalid"))
  }

}
