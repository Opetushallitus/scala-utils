package fi.vm.sade.utils.kayttooikeus

case class KayttooikeusUserDetails(val roles : List[String], val oid: String)

case class KayttooikeusUserResp(val authorities : List[GrantedAuthority], val username: String)
case class GrantedAuthority(val authority : String)
