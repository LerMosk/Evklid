package parallel

import com.twitter.util.{ Await, Future, FuturePool }
import parallel.FpPoly.DivisionResult

import java.util.concurrent.Executors
import scala.annotation.tailrec

object Gcd {
  val print: Boolean = false
  case class Result(gcd: FpPoly, x: FpPoly, y: FpPoly)

  private val pool: FuturePool = FuturePool(Executors.newFixedThreadPool(3))

  def gcdSeq(p1: FpPoly, p2: FpPoly): Result =
    gcd(p1, p2) { p =>
      val Params(x0, y0, v1, x1, y1, q1, v2) = p
      val divRes                             = v1 / v2
      val x2                                 = x0 - (x1 * q1)
      val y2                                 = y0 - (y1 * q1)
      (divRes, x2, y2)
    }

  def gcdParallel(p1: FpPoly, p2: FpPoly): Result =
    gcd(p1, p2) { p =>
      val Params(x0, y0, v1, x1, y1, q1, v2) = p
      val divResF                            = pool(v1 / v2)
      val x2F                                = pool(x0 - (x1 * q1))
      val y2F                                = pool(y0 - (y1 * q1))
      Await.result(Future.join(divResF, x2F, y2F))
    }

  case class Params(x0: FpPoly, y0: FpPoly, v1: FpPoly, x1: FpPoly, y1: FpPoly, q1: FpPoly, v2: FpPoly)

  def gcd(p1: FpPoly, p2: FpPoly)(f: Params => (DivisionResult, FpPoly, FpPoly)): Result = {
    require(p1.p == p2.p, s"p is not equal: $p1 and $p2")
    printHead()
    val p     = p1.p
    val empty = FpPoly(p)

    @tailrec
    def gcdInner(x0: FpPoly, y0: FpPoly, v1: FpPoly, x1: FpPoly, y1: FpPoly, q1: FpPoly, v2: FpPoly): Result = {
      print(q1, x1, y1, v1)
      v2.coeffs.size match {
        case 1 if v2.coeffs.head == 0 =>
          print(empty, empty, empty, v2)
          Result(v1.norm, x1, y1)
        case 1 =>
          val x2 = x0 - (x1 * q1)
          val y2 = y0 - (y1 * q1)
          print(empty, x2, y2, v2)
          Result(v2, x2, y2)
        case _ =>
          val (divRes, x2, y2) = f(Params(x0, y0, v1, x1, y1, q1, v2))
          gcdInner(x1, y1, v2, x2, y2, divRes.div, divRes.remain)
      }
    }

    val poly0          = FpPoly(p, 0)
    val poly1          = FpPoly(p, 1)
    val (pBig, pSmall) = if (p1.compareTo(p2) > 0) (p1, p2) else (p2, p1)
    print(poly0, poly1, poly0, pBig)
    val divRes = pBig / pSmall
    gcdInner(poly1, poly0, pSmall, poly0, poly1, divRes.div, divRes.remain)
  }

  def printHead(): Unit =
    if (print) println(f"\n${"q"}%30s ${"x"}%30s ${"y"}%30s ${"v"}%30s")

  def print(q: FpPoly, x: FpPoly, y: FpPoly, v: FpPoly): Unit =
    if (print) println(f"${q.toString}%30s ${x.toString}%30s ${y.toString}%30s ${v.toString}%30s")
}
