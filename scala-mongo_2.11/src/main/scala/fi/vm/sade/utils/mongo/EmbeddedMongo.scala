package fi.vm.sade.utils.mongo

import de.flapdoodle.embed.mongo.config.{IMongodConfig, MongoCmdOptionsBuilder, MongodConfigBuilder, Net, RuntimeConfigBuilder}
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.mongo.{Command, MongodStarter}
import de.flapdoodle.embed.process.config.io.ProcessOutput
import de.flapdoodle.embed.process.runtime.Network
import fi.vm.sade.utils.slf4j.Logging
import fi.vm.sade.utils.tcp.{PortChecker, PortChooser}

object EmbeddedMongo extends Logging {

  def start(portChooser: PortChooser): Option[MongoServer] = {
    if (PortChecker.isFreeLocalPort(portChooser.chosenPort)) {
      logger.info("Starting embedded mongo on port " + portChooser.chosenPort)
      Some(new MongoServer(portChooser.chosenPort))
    } else {
      logger.info("Not starting embedded mongo, seems to be running on port " + portChooser.chosenPort)
      None
    }
  }

  def withEmbeddedMongo[T](portChooser: PortChooser)(f: => T): T = {
    val mongoServer = start(portChooser)
    try {
      f
    } finally {
      mongoServer.foreach(_.stop())
    }
  }
}

class MongoServer(val port: Int) {
  private val mongodConfig: IMongodConfig = new MongodConfigBuilder()
    .version(Version.Main.PRODUCTION)
    .cmdOptions(new MongoCmdOptionsBuilder()
    		.useStorageEngine("ephemeralForTest")
    		.build())
    .net(new Net(port, Network.localhostIsIPv6))
    .build
  private val runtimeConfig = new RuntimeConfigBuilder()
    .defaults(Command.MongoD)
    .processOutput(ProcessOutput.getDefaultInstanceSilent)
    .build();
  private val runtime: MongodStarter = MongodStarter.getInstance(runtimeConfig)
  private val mongodExecutable = runtime.prepare(mongodConfig)
  private val mongod = mongodExecutable.start

  def stop() {
    mongod.stop()
    mongodExecutable.stop()
  }
}
