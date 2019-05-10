package fi.vm.sade.scala.future

import scala.concurrent.{ExecutionContext, Future, Promise}

object OphFutures {
  /**
    * Executes both futures concurrently and returns the first result that is true.
    * This can be used for example to check permissions from two different sources, if OK from either
    * source is equally good.
    *
    * Note that some failures of <code>l</code> and <code>r</code> will not be seen in the resulting future:
    * <ul>
    *   <li>if both produce a failure, only arbitrarily chosen one of them will emitted by the resulting future</li>
    *   <li>if only one produces a failure, the resulting future will emit the result from the successful parameter,
    *       and the failure will not be seen.</li>
    * <ul>
    *
    * @return fastest result of <code>l</code> or <code>r</code> that is true, or false if neither is true.
    */
  def parallelOr(l: Future[Boolean], r: Future[Boolean])(implicit ec: ExecutionContext): Future[Boolean] = {
    race(futureOr(l, r, (x: Boolean) => x), futureOr(r, l, (x: Boolean) => x))
  }

  /**
    * Executes both futures concurrently and returns the first result that satisfies <code>accept</code>.
    *
    * @return fastest result of <code>l</code> or <code>r</code> that satisfies <code>accept</code>,
    *         or arbitrary result if neither satisfies <code>accept</code>.
    */
  private[future] def parallelOr[T](l: Future[T], r: Future[T], accept: T => Boolean)(implicit ec: ExecutionContext): Future[T] = {
    race(futureOr(l, r, accept), futureOr(r, l, accept))
  }

  /**
    * @return a future that returns the fastest result of either <code>f</code> or <code>g</code>
    */
  def race[T](f: Future[T], g: Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val p = Promise[T]()
    f.recoverWith({ case _ => g }).onComplete(p.tryComplete)
    g.recoverWith({ case _ => f }).onComplete(p.tryComplete)
    p.future
  }

  /**
    * @return the result of <code>l</code> if it satisfies <code>accept</code>, or the result of <code>r</code> otherwise.
    */
  def futureOr[T](l: Future[T], r: Future[T], accept: T => Boolean)(implicit ec: ExecutionContext): Future[T] = {
    l.flatMap { result =>
      if (accept(result)) {
        Future.successful(result)
      } else {
        r
      }
    }
  }
}
