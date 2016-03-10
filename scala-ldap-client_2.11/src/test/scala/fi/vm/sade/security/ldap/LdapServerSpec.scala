package fi.vm.sade.security.ldap

import com.unboundid.ldap.sdk.{Filter, LDAPSearchException}
import org.scalatest.{FlatSpec, Matchers}

class LdapServerSpec extends FlatSpec with Matchers {
  behavior of "LdapDirectory"

  it should "get a connection" in {
    val server = TestServer("cn=TestUser", "salasana")
    server.start

    val ldap = new LdapDirectory(LdapConfig("127.0.0.1", "cn=TestUser", "salasana", port = server.port))
    ldap.withConnection { connection =>
      connection.isConnected should be(true)
    }
    server.shutDown
  }


  it should "be able to search for entries in a sub tree" in {
    withDirectory { ldap =>
      val filter = Filter.createEqualityFilter("sn", "Sukunimi")
      val ids = ldap.subTree("ou=People,dc=opintopolku,dc=fi").find(filter, "uid") {
        _.map(_.getAttribute("uid").getValue)
      }

      ids should equal(List("kayttaja", "kayttaja2"))
    }
  }

  it should "be able to search for single entry in a sub tree" in {
    withDirectory { ldap =>
      val filter = Filter.createEqualityFilter("uid", "kayttaja")
      val name = ldap.subTree("ou=People,dc=opintopolku,dc=fi").findOnly(filter, "cn") {
        _.get.getAttribute("cn").getValue
      }
      name should equal("Etunimi Sukunimi")
    }
  }

  it should "return None for  non existing single entry in a sub tree" in {
    withDirectory { ldap =>
      val filter = Filter.createEqualityFilter("uid", "kayttaja3")
      ldap.subTree("ou=People,dc=opintopolku,dc=fi").findOnly(filter, "cn") {
        _ should be(None)
      }
    }
  }


  it should "fail when directory has multiple results while searching for single entry" in {
    withDirectory { ldap =>
      val filter = Filter.createEqualityFilter("sn", "Sukunimi")
      intercept[LDAPSearchException] {
        ldap.subTree("ou=People,dc=opintopolku,dc=fi").findOnly(filter, "uid") {
          _ =>
        }
      }
    }
  }

  private def withDirectory[T](f: LdapDirectory => T): T = {
    val server = TestServer("cn=TestUser", "salasana")
    server.add("kayttaja", "sn" -> "Sukunimi", "cn" -> "Etunimi Sukunimi")
    server.add("kayttaja2", "sn" -> "Sukunimi", "cn" -> "Etunimi2 Sukunimi")

    server.start
    f (new LdapDirectory(LdapConfig("127.0.0.1", "cn=TestUser", "salasana", port = server.port)))
  }
}
