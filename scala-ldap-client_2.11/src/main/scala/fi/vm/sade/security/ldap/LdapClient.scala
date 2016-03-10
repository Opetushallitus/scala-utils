package fi.vm.sade.security.ldap

import org.json4s.DefaultFormats
import com.unboundid.ldap.sdk.Filter
import org.json4s.jackson.Serialization._
import org.slf4j.{LoggerFactory, Logger}

class LdapClient(config: LdapConfig) extends DirectoryClient {
  val logger: Logger = LoggerFactory.getLogger(getClass())
  private val peoplePath: String = "ou=People,dc=opintopolku,dc=fi"
  implicit val formats = DefaultFormats
  lazy val ldap = new LdapDirectory(config)

  def findUser(userid: String) = {
    // NOTE equality filter does the escaping
    val filter = Filter.createEqualityFilter("uid", userid)
    ldap.subTree(peoplePath).findOnly(filter, "description", "uid", "sn", "givenName", "employeeNumber") {
      searchEntry => searchEntry.map { result =>
        val roles: List[String] = Option(result.getAttribute("description").getValue).toList.flatMap { description =>
          read[List[String]](description)
        }
        def f(field: String) = Option(result.getAttribute(field)).map(_.getValue).getOrElse("")
        LdapUser(roles, f("sn"), f("givenName"), f("employeeNumber"))
      }
    }
  }

  def authenticate(userid: String, password: String) = {
    val encodedUserid = Filter.encodeValue(userid)
    if(!encodedUserid.equals(userid)) {
      logger.warn("authenticate: userid '" + userid + "' contains characters which had to be escaped: '" + encodedUserid + "'")
    }
    ldap.bind("uid=" + encodedUserid + "," + peoplePath, password)
  }
}