package fi.vm.sade.security.ldap

/**
 * LDAP user information. Currently just a list of roles.
 *
 * @param roles
 */
case class LdapUser(roles: List[String], lastName: String, givenNames: String, oid: String) {
  def hasRole(role: String) = {
    roles.contains(role)
  }
}
