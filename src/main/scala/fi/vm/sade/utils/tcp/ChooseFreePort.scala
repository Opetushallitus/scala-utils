package fi.vm.sade.utils.tcp

class ChooseFreePort(systemPropertyName: String) extends PortChooser {
  lazy val chosenPort = PortChecker.findFreeLocalPort
}
