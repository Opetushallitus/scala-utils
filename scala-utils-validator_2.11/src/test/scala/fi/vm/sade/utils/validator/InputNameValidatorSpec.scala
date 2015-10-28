package fi.vm.sade.utils.validator

import fi.vm.sade.utils.validator.InputNameValidator._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

import scala.language.implicitConversions
import scalaz.{Success, Failure, ValidationNel}

class InputNameValidatorSpec extends FlatSpec with Matchers {
  behavior of "InputNameValidator"

  it should "validate name with space" in {
    validate("Jussi Pekka").isSuccess should be (true)
  }

  it should "validate name with special letters" in {
    validate("Åke").isSuccess should be (true)
  }

  it should "not validate other than Latin-1 characters" in {
    validate("鱼").isFailure should be (true)
  }

  it should "not validate name with numbers" in {
    validate("Lauri10").isFailure should be (true)
  }
}
