package fi.vm.sade.scala.future

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit.MILLISECONDS

import fi.vm.sade.scala.future.OphFutures.parallelOr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

class OphFuturesSpec extends FlatSpec with Matchers {
  private implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(20))

  behavior of "OphFutures"

  it should "execute futures in parallel and return fastest true or otherwise false" in {
    var fastTrueCalls: Int = 0
    var fastFalseCalls: Int = 0
    var slowTrueCalls: Int = 0
    var slowFalseCalls: Int = 0

    val fastWaitMillis = 2
    val slowWaitMillis = 100

    def fastTrue: Future[(Boolean, String)] = Future {
      fastTrueCalls = fastTrueCalls + 1
      Thread.sleep(fastWaitMillis)
      (true, "fastTrue")
    }
    def fastFalse: Future[(Boolean, String)] = Future {
      fastFalseCalls = fastFalseCalls + 1
      Thread.sleep(fastWaitMillis)
      (false, "fastFalse")
    }

    def slowTrue: Future[(Boolean, String)] = Future {
      slowTrueCalls = slowTrueCalls + 1
      Thread.sleep(slowWaitMillis)
      (true, "slowTrue")
    }

    def slowFalse: Future[(Boolean, String)] = Future {
      slowFalseCalls = slowFalseCalls + 1
      Thread.sleep(slowWaitMillis)
      (false, "slowFalse")
    }

    val accept: ((Boolean, String)) => Boolean = { x: (Boolean, String) => x._1 }
    implicit val timeout: FiniteDuration = Duration(10 * slowWaitMillis, MILLISECONDS)

    // See documentation of OphFutures#parallelOr about the race condition.
    //run(parallelOr(fastFalse, slowFalse, accept)) should equal((false, "slowFalse"))
    run(parallelOr(fastFalse, slowFalse, accept))._1 should equal(false)
    run(parallelOr(fastFalse, slowTrue, accept)) should equal((true, "slowTrue"))
    run(parallelOr(slowFalse, fastTrue, accept)) should equal((true, "fastTrue"))
    run(parallelOr(fastTrue, slowFalse, accept)) should equal((true, "fastTrue"))
    run(parallelOr(fastTrue, fastFalse, accept)) should equal((true, "fastTrue"))
    run(parallelOr(fastFalse, fastTrue, accept)) should equal((true, "fastTrue"))
    run(parallelOr(slowFalse, slowTrue, accept)) should equal((true, "slowTrue"))
    run(parallelOr(slowTrue, slowFalse, accept)) should equal((true, "slowTrue"))
    run(parallelOr(slowTrue, fastTrue, accept)) should equal((true, "fastTrue"))

    fastTrueCalls + fastFalseCalls + slowTrueCalls + slowFalseCalls should equal(18)

    run(parallelOr(
      Future {
        Thread.sleep(slowWaitMillis)
        true
      },
      Future {
        Thread.sleep(fastWaitMillis)
        false
      })) should equal(true)
  }

  private def run[T](future: Future[T])(implicit timeout: FiniteDuration): T = Await.result(future, timeout)
}
