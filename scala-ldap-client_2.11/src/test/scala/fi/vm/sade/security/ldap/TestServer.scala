package fi.vm.sade.security.ldap

import com.unboundid.ldap.listener.{InMemoryDirectoryServer, InMemoryDirectoryServerConfig}
import com.unboundid.ldap.sdk.Attribute

case class TestServer(user:String, pw:String) {
  val config = new InMemoryDirectoryServerConfig("dc=opintopolku,dc=fi")
  config.addAdditionalBindCredentials(user, pw)

  var ds:Option[InMemoryDirectoryServer]  = None
  var users:Map[String, Seq[(String,String)]] = Map()

  def add(uid: String, attributes: (String, String)*) = users = users + (uid -> attributes)

  def start  = {
    ds = Some(new InMemoryDirectoryServer(config))
    for (
      directory <- ds
    ) {
      directory.add("dc=opintopolku,dc=fi", new Attribute("objectclass", "top"), new Attribute("objectclass" , "domain"))
      directory.add("ou=People,dc=opintopolku,dc=fi", new Attribute("ou", "people"), new Attribute("objectclass", "organizationalunit"))
      for (
        (uid, attribs) <- users
      ) {
        val attributes = new Attribute("objectClass", "uidObject") +: new Attribute("objectClass", "inetOrgPerson")  +: attribs.map{ case (name, value) => new Attribute(name, value)}
        import scala.collection.JavaConversions._
        directory.add(s"uid=$uid,ou=People,dc=opintopolku,dc=fi", attributes)
      }
      directory.startListening()

    }
  }


  def shutDown = {
    ds.get.shutDown(true)
  }
  def port = ds.get.getListenPort
}

