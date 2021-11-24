package parallel

import com.aparapi.Kernel
import com.twitter.util.{ Future, FuturePool }
import parallel.Utils.CollectableFuture

import java.util.concurrent.{ ConcurrentHashMap, Executors }
import scala.jdk.CollectionConverters._

object Parallel {
  def calcCPU[A](threads: Int, args: Seq[A])(f: (A, A) => A): Future[A] = {
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

  def calcGPU[A](threads: Int, args: Seq[A])(f: (A, A) => A): A = {
    require(args.nonEmpty, "args is empty")
    val firstStepResult =
      if (args.size > threads) {
        val batched = split(threads, args)
        firstStepGPU(batched.toIndexedSeq, f)
      } else args

    def secondStep(values: Seq[A]): Seq[A] =
      values.size match {
        case 1 => values
        case v if v % 2 == 0 =>
          val tuples = values.grouped(2).map(seq => (seq.head, seq(1))).toIndexedSeq
          secondStepGPU(tuples, f)
        case _ =>
          val tuples = values.tail.grouped(2).map(seq => (seq.head, seq(1))).toIndexedSeq
          secondStepGPU(tuples, f) :+ values.head
      }

    secondStep(firstStepResult).head
  }

  private def firstStepGPU[A](values: IndexedSeq[Seq[A]], f: (A, A) => A): Seq[A] =
    execGPU(values)(_.reduce(f))

  private def secondStepGPU[A](values: IndexedSeq[(A, A)], f: (A, A) => A): Seq[A] =
    execGPU(values) { case (v1, v2) => f(v1, v2) }

  private def execGPU[B, A](values: IndexedSeq[B])(f: B => A): Seq[A] = {
    val result = new ConcurrentHashMap[Int, A]()

    val kernel: Kernel = new Kernel {
      override def run(): Unit = {
        val i = getGlobalId()
        result.put(i, f(values(i)))
      }
    }
    kernel.addExecutionModes(Kernel.EXECUTION_MODE.GPU)
    kernel.execute(values.size)
    result.values().asScala.toSeq
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
