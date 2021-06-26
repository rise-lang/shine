package rise.autotune

import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

object autoTuningUtils {

  // WARNING: does not kill the thread, we only return to host program
  // Thread.stop() is deprecated and cannot be used here
  def runWithTimeout[T](timeoutMs: Long)(f: => T) : Option[T] = {
    try {
      val executor = Executors.newSingleThreadExecutor()
      val res = executor.invokeAll(java.util.Arrays.asList(new java.util.concurrent.Callable[T] {
        override def call(): T = f
      }), timeoutMs, TimeUnit.MILLISECONDS)
      executor.shutdown()
      Some(res.get(0).get())
    } catch {
      case _: TimeoutException => None
      case _: java.util.concurrent.CancellationException => None
    }
  }
}
