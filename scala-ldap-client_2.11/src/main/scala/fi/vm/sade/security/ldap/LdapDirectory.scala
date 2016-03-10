package fi.vm.sade.security.ldap

import com.unboundid.ldap.sdk._

protected[ldap] class LdapDirectory(config: LdapConfig) {
  def subTree(baseDN: String) = new SearchSc(this, baseDN, SearchScope.SUB)

  def bind(userDn: String, password: String): Boolean = withConnection { connection =>
    try {
      val result: BindResult = connection.bind(userDn, password)
      result.getResultCode == ResultCode.SUCCESS
    } catch {
      case e: LDAPException => false
    }
  }

  def withConnection[R](f: (LDAPConnection) => R): R = {
    val connection: LDAPConnection = new LDAPConnection()
    connection.connect(config.host, config.port)
    try {
      connection.bind(config.userDn, config.password)
      f(connection)
    } finally {
      connection.close()
    }
  }
}

protected[ldap] class SearchSc(ldap: LdapDirectory, baseDN: String, scope: SearchScope) {
  private def sr(filter: Filter, attributes: Seq[String]) = new SearchRequest(baseDN, scope, filter, attributes:_*)

  def find[R](filter: Filter, attributes: String*)(f: (List[SearchResultEntry]) => R):R =
    ldap withConnection{
      connection =>
        import scala.collection.JavaConversions._
        f(connection.search(sr(filter, attributes)).getSearchEntries.toList)
    }

  def findOnly[R](filter: Filter, attributes: String*)(f: (Option[SearchResultEntry]) => R): R =
    ldap withConnection{
      connection =>
        f(Option(connection.searchForEntry(sr(filter, attributes))))
    }
}

case class LdapConfig(host: String, userDn: String, password: String, port: Int = 389)
