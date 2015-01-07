package fi.vm.sade.utils.cas

trait TicketClient {
  def validateServiceTicket(ticket: CasTicket): CasResponse
  def getServiceTicket(service: CasTicketRequest): Option[String]
}
