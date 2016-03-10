package fi.vm.sade.security.ldap

import org.scalatest.{FlatSpec, Matchers}

class LdapClientSpec extends FlatSpec with Matchers {
  behavior of "LdapClient"
  val server  = TestServer("cn=TestUser", "salasana")
  server.add("käyttäjä",
             "userPassword" -> "password",
             "sn" -> "Kayttaja",
             "cn" -> "Testi Kayttaja",
              "description" -> "[\"SERVICE1_ORG_CRUD\",\"SERVICE2_ORG_CRUD\"]")
  server.start
  val ldap = new LdapClient(LdapConfig("127.0.0.1", "cn=TestUser", "salasana", port = server.port))

  it should "find roles of a OPH user" in {
    ldap.findUser("käyttäjä").get should be (LdapUser(List("SERVICE1_ORG_CRUD", "SERVICE2_ORG_CRUD"), "Kayttaja", "", ""))
  }

  it should "should authenticate ok with right password" in {
    ldap.authenticate("käyttäjä", "password") should be (true)
  }

  it should "should fail to authenticate with wrong password" in {
    ldap.authenticate("käyttäjä", "Password!") should be (false)
  }

  // check log manually
  it should "should sanitize userid input in authenticate" in {
    ldap.authenticate("*)(uid=*))(|(uid=*", "password") should be (false)
  }
}
