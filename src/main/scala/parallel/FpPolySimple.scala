package parallel

import parallel.FpPolySimple.{ round, DivisionResult }
import parallel.Utils.power

import scala.annotation.tailrec
import scala.util.Random

class FpPolySimple private (val p: Int, val coeffs: Seq[Int]) extends Comparable[FpPolySimple] {

  def +(other: FpPolySimple): FpPolySimple =
    bitwiseOperation(other) { (i1, i2) =>
      (i1 + i2) % p
    }

  def -(other: FpPolySimple): FpPolySimple =
    bitwiseOperation(other) { (i1, i2) =>
      val v = (i1 - i2) % p
      if (v < 0) v + p else v
    }

  def *(i: Int): FpPolySimple = {
    val res = coeffs.map(v => (i * v) % p)
    fpPoly(res)
  }

  def *(other: FpPolySimple): FpPolySimple = {
    require(other.p == p, s"p is not equal: $this and $other")
    val res = coeffs.reverse.zipWithIndex
      .map {
        case (v, i) => fpPoly((other * v).coeffs ++ Seq.fill(i)(0))
      }
    res.reduceLeft(_ + _)
  }

  def /(divider: FpPolySimple): DivisionResult = {
    require(divider.p == p, s"p is not equal: $this and $divider")

    @tailrec
    def moveRanks(divRemain: Seq[Int], numberRemain: Seq[Int], movedRanks: Int): (Seq[Int], Int) = {
      val r = divRemain.dropWhile(_ == 0)
      r.size match {
        case v if v == divider.size                    => (r ++ numberRemain, movedRanks)
        case v if v + numberRemain.size < divider.size => (r ++ numberRemain, movedRanks + 1 + numberRemain.size)
        case _ if numberRemain.isEmpty                 => (r, movedRanks)
        case _                                         => moveRanks(divRemain :+ numberRemain.head, numberRemain.tail, movedRanks + 1)
      }
    }

    @tailrec
    def div(xPrev: Seq[Int], res: Seq[Int]): (Seq[Int], Seq[Int]) =
      xPrev.size - divider.size match {
        case v if v < 0 => (xPrev, res)
        case _ =>
          val (work, wait)        = xPrev.splitAt(divider.size)
          val i                   = (work.head * inverse(divider.coeffs.head)) % p
          val remain              = fpPoly(work) - (divider * i)
          val (xNext, movedRanks) = moveRanks(remain.coeffs, wait, -1)
          div(xNext, res ++ (i +: Seq.fill(movedRanks)(0)))
      }

    val (remain, divRes) = div(coeffs, Seq.empty)
    DivisionResult(fpPoly(remain), fpPoly(divRes))
  }

  def bitwiseOperation(other: FpPolySimple)(f: (Int, Int) => Int): FpPolySimple = {
    require(other.p == p, s"p is not equal: $this and $other")
    val res = zip(coeffs, other.coeffs)
    val res2 = res.map {
      case (i1, i2) => f(i1, i2)
    }
    fpPoly(res2)
  }

  def fpPoly(coeffs: Seq[Int]): FpPolySimple = FpPolySimple(p, round(coeffs): _*)

  def zip(x1: Seq[Int], x2: Seq[Int]): Seq[(Int, Int)] = x1.size - x2.size match {
    case 0          => x1.zip(x2)
    case v if v < 0 => (Seq.fill(-v)(0) ++ x1).zip(x2)
    case v          => x1.zip(Seq.fill(v)(0) ++ x2)
  }

  def size: Int = coeffs.size

  def inverse(v: Int): Int = power(v, p - 2, p)

  def norm: FpPolySimple =
    if (coeffs.head != 1 || coeffs.head != 0) {
      val normalizer = inverse(coeffs.head)
      val normalized = 1 +: coeffs.tail.map(v => (v * normalizer) % p)
      fpPoly(normalized)
    } else this

  override def toString: String = s"[${coeffs.mkString(", ")}]"

  def compareTo(t: FpPolySimple): Int = {
    @tailrec
    def cmp(values: Seq[(Int, Int)]): Int =
      values.headOption match {
        case None                           => 0
        case Some((i1, i2)) if i1 - i2 == 0 => cmp(values.tail)
        case Some((i1, i2))                 => i1.compare(i2)
      }
    coeffs.size - t.coeffs.size match {
      case 0 => cmp(coeffs.zip(t.coeffs))
      case v => v
    }
  }

}

object FpPolySimple {
  val r: Random = Random
  def apply(p: Int, coeffs: Int*): FpPolySimple = {
    require(coeffs.forall(_ < p), s"Coefficient not leather than $p")
    new FpPolySimple(p, round(coeffs))
  }

  def round(s: Seq[Int]): Seq[Int] = if (s.forall(_ == 0)) Seq(0) else s.dropWhile(_ == 0)

  def gen(p: Int, maxDeg: Int): FpPolySimple = {
    val deg    = r.nextInt(maxDeg + 1)
    val coeffs = Seq.fill(deg + 1)(r.nextInt(p))
    apply(p, coeffs: _*)
  }

  case class DivisionResult(remain: FpPolySimple, div: FpPolySimple)
}
