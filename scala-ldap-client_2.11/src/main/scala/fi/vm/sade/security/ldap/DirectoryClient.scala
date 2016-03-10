package fi.vm.sade.security.ldap

trait DirectoryClient {
  def findUser(userid: String): Option[LdapUser]
  def authenticate(userid: String, password: String): Boolean
}
