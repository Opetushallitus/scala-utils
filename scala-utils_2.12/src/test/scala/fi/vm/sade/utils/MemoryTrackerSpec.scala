package fi.vm.sade.utils

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MemoryTrackerSpec extends Specification {
  "MemoryTracker" should {
    "track consumed memory of given code block" in {
      val bytesToAllocate: Long = Runtime.getRuntime.maxMemory() / 10
      val result: (Array[Byte], Long) = MemoryTracker.memoryUsage(s"Create array of $bytesToAllocate bytes") {
        new Array[Byte](bytesToAllocate.toInt)
      }
      val consumedBytes: Long = result._2
      consumedBytes must be_>=(bytesToAllocate / 2) // For some reason, there increased memory use is never that much
      result._1.length.must_==(bytesToAllocate)
    }
  }
}
