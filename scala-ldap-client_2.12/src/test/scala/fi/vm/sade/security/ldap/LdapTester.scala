package fi.vm.sade.security.ldap

object LdapTester extends App {
  val user = new LdapClient(LdapConfig("ldap.qa.oph.ware.fi", "cn=admin,ou=People,dc=opintopolku,dc=fi", "xKypu8cMBN")).findUser("ainrauti")
  println(user)
}
