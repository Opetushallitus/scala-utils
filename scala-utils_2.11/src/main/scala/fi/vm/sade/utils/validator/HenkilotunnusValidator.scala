package fi.vm.sade.utils.validator

import java.text.SimpleDateFormat

import scala.language.postfixOps
import scala.util.Try
import scalaz.Scalaz._
import scalaz._

object HenkilotunnusValidator {

  private val birthdayFormat = "yyyy-MM-dd"
  private val checksumCharacters = "0123456789ABCDEFHJKLMNPRSTUVWXY"
  private val formCheck = "^[0-9]{6}.[0-9]{3}[0-9ABCDEFHJKLMNPRSTUVWXY]$"

  def validate(henkilotunnus: String): ValidationNel[String, String] = {
    def validateLength: ValidationNel[String, String] = {
      if (henkilotunnus.length != 11)
        s"length is not 11, but ${henkilotunnus.length}".failureNel
      else
        henkilotunnus.successNel
    }

    def validateBirthday: ValidationNel[String, String] = {
      if (henkilotunnus.length == 11) {
        "([0-9]{2})".r findAllIn henkilotunnus.take(6) toList match {
          case l: List[String] if l.length != 3 => s"cannot parse birthday part ${henkilotunnus.substring(0, 7)}".failureNel
          case List(day: String, month: String, year: String) =>
            val century = henkilotunnus.charAt(6) match {
              case '+' => "18"
              case '-' => "19"
              case 'A' => "20"
              case c => return s"invalid separator character $c".failureNel
            }
            val birthday = s"$century$year-$month-$day"
            Try{
              val parser = new SimpleDateFormat(birthdayFormat)
              parser.setLenient(false)
              parser.parse(birthday)
            }.toOption match {
              case Some(_) => henkilotunnus.successNel
              case None => s"cannot parse birthday $birthday".failureNel
            }
        }
      } else {
        henkilotunnus.successNel
      }
    }

    def validateChecksum: ValidationNel[String, String] = {
      if (henkilotunnus.matches(formCheck)) {
        val mod: Int = (henkilotunnus.substring(0, 6) + henkilotunnus.substring(7, 10)).toInt % checksumCharacters.length
        if (henkilotunnus.charAt(10) == checksumCharacters.charAt(mod)) henkilotunnus.successNel
        else s"checksum character ${henkilotunnus.charAt(10)} is invalid".failureNel
      } else {
        henkilotunnus.successNel
      }
    }

    (validateLength |@| validateBirthday |@| validateChecksum)((lengthCheck, birthdayCheck, checksumCheck) => henkilotunnus)
  }

  def isValid(henkilotunnus: String): Boolean = validate(henkilotunnus).isSuccess

}
