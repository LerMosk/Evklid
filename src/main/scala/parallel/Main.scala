package parallel

import com.twitter.util.Await

object Main extends App {
  val p         = 997
  val polyCount = 10000
  val threads   = 4
  val maxDeg    = 500

  val common = FpPoly(p, coeffs = 1, 94, 61, 5, 111, 4)
  println(s"gcd = $common")
  val polys = Seq.fill(polyCount)(FpPoly.gen(p, maxDeg) * common)
  println("Calculate gcd for:")
  println(s"p = $p")
  polys.zipWithIndex.foreach { case (poly, i) => println(s"p$i = $poly") }

  val gcd: (FpPoly, FpPoly) => FpPoly = Gcd(printCalculations = false).gcdSeq(_, _).gcd

  println("Sequential algorithm")
  withCalkTime {
    val result = polys
      .reduceOption(gcd)
      .getOrElse(throw new IllegalArgumentException("polys is empty"))
    println(result)
  }

  println("Parallel algorithm CPU")
  withCalkTime {
    val result = Await.result(Parallel.calcCPU(threads, polys)(gcd))
    println(result)
  }

  println("Parallel algorithm GPU")
  withCalkTime {
    val result = Parallel.calcGPU(threads, polys)(gcd)
    println(result)
  }

  def withCalkTime(f: => Unit): Unit = {
    val t1 = System.currentTimeMillis()
    f
    val t2 = System.currentTimeMillis()
    println(s"It took ${t2 - t1} ms")
  }
}
