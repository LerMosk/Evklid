package parallel

import com.twitter.util.{ Future, FuturePool }

import java.util.concurrent.Executors

object Parallel {
  def calc[A](threads: Int, args: Seq[A])(f: (A, A) => A): Future[A] = {
    require(args.nonEmpty, "args is empty")
    val pool: FuturePool = FuturePool(Executors.newFixedThreadPool(threads))
    val firstStepResult =
      if (args.size > threads)
        split(threads, args)
          .map(batch => pool(batch.reduce(f)))
          .collectAll
      else Future.value(args)

    def secondStep(values: Seq[A]): Future[Seq[A]] =
      values.size match {
        case 1 => Future.value(values)
        case _ =>
          values
            .grouped(2)
            .map {
              case Seq(v1, v2) => pool(f(v1, v2))
              case Seq(v1)     => Future.value(v1)
            }
            .collectAll
            .flatMap(secondStep)
      }

    firstStepResult.flatMap(secondStep).map(_.head)
  }

  implicit class CollectableFuture[A](v: Iterator[Future[A]]) {
    def collectAll: Future[Seq[A]] = Future.collect(v.toSeq)
  }

  private def split[A](buckets: Int, args: Seq[A]): Iterator[Seq[A]] = {
    val rounded = args.size / buckets
    args.size % buckets match {
      case 0 => args.grouped(args.size / buckets)
      case v =>
        val (first, second) = args.splitAt((rounded + 1) * v)
        first.grouped(rounded + 1) ++ second.grouped(rounded)
    }
  }

}
