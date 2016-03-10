package fi.vm.sade.security.ldap

class MockDirectoryClient(users: Map[String, LdapUser]) extends DirectoryClient {
  override def findUser(userid: String) = users.get(userid)

  override def authenticate(userid: String, password: String) = users.contains(userid)
}