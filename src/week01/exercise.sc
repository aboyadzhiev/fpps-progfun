object exercise {

  /**
   * Implementation of Newton–Raphson method for finding successively
   * better approximations to the roots (or zeroes) of a real-valued function.
   *
   * @param x Double
   *
   * @return Double the approximations root of x
   */
  def sqrt(x: Double): Double = {
    def abs(x: Double) = if (x < 0) -x else x

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.00001

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    sqrtIter(1, x)
  }

  sqrt(144)


  /**
   * Calculate factorial of n using tail recursion
   *
   * @param n Int
   *
   * @return
   */
  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int = {
      if(n == 0) acc
      else loop(acc * n, n - 1)
    }
    loop(1, n)
  }

  factorial(5)
}