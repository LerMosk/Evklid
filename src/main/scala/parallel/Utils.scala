package parallel

object Utils {
  def power(x0: Int, y0: Int, p: Int): Int = {
    var res: Int = 1 // Initialize result

    var x = x0 % p // Update x if it is more than or
    // equal to p

    if (x0 == 0) return 0 // In case x is divisible by p;

    var y = y0

    while (y > 0) {
      // If y is odd, multiply x with result
      if ((y & 1) != 0) res = (res * x) % p

      // y must be even now
      y = y >> 1 // y = y/2
      x = (x * x) % p
    }
    res
  }
}
