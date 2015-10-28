package fi.vm.sade.utils.validator

import scalaz.Scalaz._
import scalaz._

object InputNameValidator {
  private val LATIN_1_NAME_REGEX = "^$|^[a-zA-ZÀ-ÖØ-öø-ÿ]$|^[a-zA-ZÀ-ÖØ-öø-ÿ'][a-zA-ZÀ-ÖØ-öø-ÿ ,-.']*(?:[a-zA-ZÀ-ÖØ-öø-ÿ.']+$)$";

  def validate(name: String): ValidationNel[String, String] = {
    if(name matches(LATIN_1_NAME_REGEX)) {
      name.successNel
    } else {
      s"Invalid name ${name}! Name should only contain Latin-1 subset of UTF-8 characters.".failureNel
    }
  }
}
