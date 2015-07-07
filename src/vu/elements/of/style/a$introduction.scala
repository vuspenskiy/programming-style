package vu.elements.of.style

/**
 * @author v.uspenskiy
 * @since 25/06/15
 */

object a$introduction {

  def fragment(N: Int) {
    val V = for (i <- 0 until N) yield
    for (j <- 0 until N) yield
    (i / j) * (j / i)
  }

  // The program fragment puts 1 on the diagonal on V and zeros everywhere else. How clever!


  def aBetterVersion(N: Int) {

    for (i <- 0 until N) yield {
      for (j <- 0 until N) yield if (i == j) 1.00 else 0.00
    }
  }

  // It is more important to make a purpose of a program unmistakable
  // than to show virtuosity. Setting up an identity matrix must be surely
  // a small part of the whole program.

  // Write clearly — don't be too clever!


  // Square root example that would profit from criticism and revision
  def poorSqrt() {
    val x = Console.readDouble()

    var a = x / 2
    var b = 0.00
    var c = 0.00

    do {
      b = (x / a + a) / 2
      c = b - a
      if (c < 0) c = -c
      a = b
    } while (c >= 10E-6)

    Console.println(String.format("%6.1f", b))
  }

  def aBetterSqrt(x: Double) = {

    // No I/O to be able to reuse program

    var a = x / 2
    var b = x / 2

    do {
      a = b
      b = (x / a + a) / 2
    } while (math.abs(b - a) /* Library function used */ >= 1E-5 /* Literal not to be confused with 1*10^-6 */)

    b
  }
}
