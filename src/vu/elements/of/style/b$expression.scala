package vu.elements.of.style

/**
 * @author v.uspenskiy
 * @since 25/06/15
 */

object b$expression {

  // It is easy to mistake a sequence of overly-simple expressions for profundity:

  def aSophisticatedLargest(a: Int, b: Int, c: Int) = {

    var large: Int = 0

    if(a > b) {
      large = a
    } else {
      large = b
    }

    if(large > c) {

    } else {
      large = c
    }

    large
  }

  // In the end `large` contains the largest of `a`, `b` and `c`
  // 10 lines to find a biggest number

  // There is a number of ways to do this computation, e.g.

  def anIllustrativeLargest(a: Int, b: Int, c: Int) = {

    var large: Int = a
    if(b > large) large = b
    if(c > large) large = c

    large
  }

  // Say what you mean, simply and directly!


  def aBetterLargest(a: Int, b: Int, c: Int) = {
    math.max(a, math.max(b, c))
  }

  // Library functions let you build on the work of others,
  // instead of starting from scratch each time

  // Use library functions!


  // Peculiar modes of expression often arise of attempts to write "efficient code"

  def expression(x1: Double, x2: Double) {
    val f1 = x1 - x2 * x2
    val f2 = 1.00 - x2
    val fx = f1 * f1 + f2 * f2
    // Note that it is more efficient to compute
    // f1*f1 than to compute f1^2
    fx
  }

  // Whether "efficient" means "takes less time" or "takes less machine code",
  // the comment is not always true. Many compilers would in fact generate
  // shorter and faster code for:

  // Needed to make a point
  implicit class PowDouble(d: Double) {
    def ^(d2: Double) = math.pow(d, d2)
  }

  def inlineExpression(x1: Double, x2: Double): Unit = {
    (x1 - x2^2)^2 + (1.00 - x2)^2
  }

  // The fewer temporary variables in a program, the less chance
  // there is that one will not be properly initialized, or that one
  // will be altered unexpectedly before it is used,
  // and the easier the program is to understand

  // Avoid temporary variables!

  // A program usually has to be read several times in the process of getting it debugged.
  // The harder it is for people to grasp the intent of any given section,
  // the longer it will be before the program becomes operational.
  //
  // Trying to outsmart a compiler defeats much of the purpose of using one.

  // Write clearly — don't sacrifice clarity for "efficiency"!


  def aVariation() = {

    var no: Int = 42

    def putPage() = { /* Doing something */ }

    //  Note that `110010` in binary is `50` in decimal
    //  This will be used for line counting.

    if(no > 110010) {
      putPage()
      no = 0
    }
  }

  // The programmer evidently hopes to avoid a run-time type-conversion by using
  // binary literal. One of the first services to be automated in early computer
  // languages was the conversion of decimal to binary in compile-time.

  // Let the machine do the dirty work!


  // Repeated patterns of code catch the eye when scanning listings:

  def manipulateTriangle(x1: Double, x2: Double, x3: Double,
                         y1: Double, y2: Double, y3: Double) {
    // Compute lengths of sides
    val ab = math.sqrt(math.pow(x2 - x1, 2) + math.pow(y2 - y1, 2))
    val ac = math.sqrt(math.pow(x3 - x1, 2) + math.pow(y3 - y1, 2))
    val bc = math.sqrt(math.pow(x3 - x2, 2) + math.pow(y3 - y2, 2))
    // Compute area
    val s = (ab + bc + ac) / 2.00
    val area = math.sqrt(s * (s - bc) * (s - ac) * (s - ab))
    // Compute angles
    val alpha = math.atan((4.0 * area) / (math.pow(ac, 2) + math.pow(ab, 2) - math.pow(bc, 2)))
    val betta = math.atan((4.0 * area) / (math.pow(ab, 2) + math.pow(bc, 2) - math.pow(ac, 2)))
    val gamma = math.atan((4.0 * area) / (math.pow(ac, 2) + math.pow(bc, 2) - math.pow(ab, 2)))
  }

  // We can see immediately the advantage of defining two functions:
  def side(xa: Double, xb: Double, ya: Double, yb: Double) = {
    math.sqrt(math.pow(xa - xb, 2) + math.pow(ya - yb, 2))
  }

  def angle(sarea: Double, sa: Double, sb: Double, sc: Double) = {
    math.atan2(4.0 * sarea, math.pow(sa, 2) + math.pow(sb, 2) - math.pow(sc, 2))
  }

  // So we can write a
  def betterManipulateTriangle(x1: Double, x2: Double, x3: Double,
                               y1: Double, y2: Double, y3: Double) {
    // Compute lengths of sides
    val ab = side(x2, x1, y2, y1)
    val ac = side(x3, x1, y3, y1)
    val bc = side(x3, x2, y3, y2)
    // Compute area
    val s = (ab + bc + ac) / 2.00
    val area = math.sqrt(s * (s - bc) * (s - ac) * (s - ab))
    // Compute angles
    val alpha = angle(area, ac, ab, bc)
    val betta = angle(area, ab, bc, ac)
    val gamma = angle(area, ac, bc, ab)
  }

  // This is not only easier to write but also easier to modify.
  // For instance changing `math.atan` to `math.atan2` to avoid error when divisor is zero.

  // Another eye-catching repeat appears in,

  def fragmentWithRepeat() {

    val r = 12
    val l = 24
    var time = 0.00
    var theta = 0.00
    val delth = 2.00 * math.Pi / 100.00

    for (i <- 1 to 100) {
      val x = r * (1.00 - math.cos(theta)) + l - l * math.sqrt(1.00 - math.pow(r * math.sin(theta) / l, 2.00))
      theta = theta + delth
      val xnew = r * (1.00 - math.cos(theta)) + l - l * math.sqrt(1.00 - math.pow(r * math.sin(theta) / l, 2.00))
      val vel = (xnew - x) / 0.01
      time = time + 0.01
      Console.println(s"$time, $theta, $xnew, $vel")
    }
  }

  // Our first impulse is to define another function for the gangling expression that appears twice,
  // but closer inspection shows a more fundamental oversight.

  // Two adjacent values of x are computed twice as often as necessary, as previous value is always known.

  // The practice of incrementing a floating point variable many times might be troublesome,
  // to keep arithmetic errors from piling up, we are better off computing `time` and `theta` from i.

  def puttingEverythingTogetherGives() {

    val r = 12
    val l = 24
    var x = 0.00

    for (i <- 1 to 100) {
      val time = i.toDouble / 100.00
      val theta = 2.00 * math.Pi * time
      val xnew = r * (1.00 - math.cos(theta)) + l - l * math.sqrt(1.00 - math.pow(r * math.sin(theta) / l, 2.00))
      val vel = (xnew - x) / 0.01
      Console.println(s"$time, $theta, $xnew, $vel")
      x = xnew
    }
  }

  // Replace repetitive expressions by calls to a common function!


  // Arithmetic expressions may differ from the way we intuitively tend to write them.
  // We are accustomed, in writing algebra, to bind multiplication tighter than division.

  // That
  def x(a: Double, b: Double, c: Double) = a * b / 2.0 * c

  // means
  def wrongX(a: Double, b: Double, c: Double) = (a * b) / (2.0 * c) // Wrong

  // when interpretation is
  def rightX(a: Double, b: Double, c: Double) = ((a * b) / 2.0) * c

  // Parenthesize to avoid ambiguity!


  // Variable names can also be either safe or dangerous:

  val N05S = 123

  // Now, was that "N, letter O, five, S" or "N, zero, five, S" or even "NOSS"?
  // Mixtures of similar characters (letter O and digit 0, letter l and digir 1, etc) are unsafe,
  // as are long identifiers that differ only at the end. Use xPos, not positionX.

  // Similar identifiers are dangerous in general,
  val n = 23
  val nn = n * n
  val nnn = n * n * n

  // It is only when, much further down, we read

  Console.println(s"$n $nn $nnn")

  // the typographical error in the second line becomes clear.

  // Choose variable names that won't be confused!


  // A conditional expression can also disguised by using,

  def unnecessaryBranches(print: Boolean): Unit = {

    if(!print) {
      // Do nothing
    } else if(!print) {
      Console.println("6,105")
    }
  }

  // This code is certainly better written as,

  def oneWayPrint(print: Boolean): Unit = {

    if(print) {
      Console.println("6,105")
    }
  }

  // Now we can tell at a glance that there is only way to reach the `println` call.

  // The influence of Fortran arithmetic IF (when there was three branches to go
  // whether condition returned -1, 0 or 1, like in Java `Comparator`)
  // often extends into misuse of the logical IF (when condition is `Boolean` type) now:

  def arithmeticalIfInfluenced(x1: Int, x2: Int): Unit = {

    if(x1 - x2 < 0) {
      Console.println("6,105")
    }
  }

  // Should be written

  def normalLogicalIf(x1: Int, x2: Int): Unit = {

    if(x1 < x2) {
      Console.println("6,105")
    }
  }

  // Avoid unnecessary branches!


  // A failure to state the underlying logic can lead to tangled control flow,

  def rudimentaryDatingService() = {
    val female = (0 until 8).map(_ => Console.readBoolean())

    while(true) {
      val male = (0 until 8).map(_ => Console.readBoolean())

      var dontPrint = false

      for (i <- 0 until 8) {

        if (female(i))
          if (!male(i))
            dontPrint = true
          else {
          }
        else
          if (!male(i)) {
          } else
            dontPrint = true
      }

      if(!dontPrint) {
        Console.println("Boy")
      }
    }
  }

  // The program is supposed to write "Boy" only if each of the `male`
  // has the same truth value as the corresponding `female`.

  // We can improve readability,

  def improvedReadabilityDatingService() = {
    val female = (0 until 8).map(_ => Console.readBoolean())

    while(true) {
      val male = (0 until 8).map(_ => Console.readBoolean())

      var dontPrint = false

      for (i <- 0 until 8) {
        if((female(i) && !male(i)) || (!female(i) && male(i))) {
          dontPrint = true
        }
      }

      if(!dontPrint) {
        Console.println("Boy")
      }
    }
  }

  // Or even

  def directEqualityCheckDatingService() = {
    val female = (0 until 8).map(_ => Console.readBoolean())

    while(true) {
      val male = (0 until 8).map(_ => Console.readBoolean())

      var dontPrint = false

      for (i <- 0 until 8) {
        dontPrint = female(i) != male(i)
      }

      if(!dontPrint) {
        Console.println("Boy")
      }
    }
  }

  // This tells us directly that the program will go on th read the next input line,
  // without printing "Boy", if any `female(i)` differ from the corresponding `male(i)`.

  // Don't use conditional branches as a substitute for a logical expression!


  // Scala gives even much more elegant way of writing it

  def scalaVersionDatingService() = {

    val female = (0 until 8).map(_ => Console.readBoolean())

    while(true) {
      val male = (0 until 8).map(_ => Console.readBoolean())

      if((male zip female) forall { case (m, f) => m == f }) {
        Console.println("Boy")
      }
    }
  }


  // Most of the time we just using a relational operator (< or ==) in IF.
  // But we can, if we wish, use Boolean operators: &&, ||, ! to make arbitrarily complex
  // logical expressions. Boolean algebra is not used nearly as widely as ordinary arithmetic,
  // so we must write logical expressions more carefully lest we confuse the reader:

  def negatedConditions(x1: Int, x2: Int, array: Array[Int], i: Int) = {

    var icount = 0

    if(x1 >= array(i)) {

    } else {
      if(array(i) < x2) icount = icount + 1
    }

    icount
  }

  // It takes a while to realize that icount is incremented only if array(i) lies between x1 and x2.
  // Inversions slow down reading comprehension and should be avoided. Rewriting gives,

  def rewrittenConditions(x1: Int, x2: Int, array: Array[Int], i: Int) = {

    var icount = 0

    if(x1 < array(i) && array(i) < x2) icount = icount + 1

    icount
  }

  // It is much easier to tell at a glance what the logic implies.

  def anotherCondition(k: Int, print: String) = {

    if(k == 0 || (!(print == "yes" || print == "no"))) {
      Console.println("do")
    }
  }

  // The inversion and double parentheses slow down comprehension. It seems better to
  // distribute the "not" operation through the parenthesized expression with De Morgan`s laws.

  def simplifiedCondition(k: Int, print: String) = {

    if(k == 0 || (print != "yes" && print != "no")) {
      Console.println("do")
    }
  }

  // The expression is still not simple, but it is now in a form that more closely resembles how we speak
  // (it is important, though, to track that both negation and or/and operation change applied).

  // If a logical expression is hard to understand, try transforming it!


  // Let's conclude with one larger example, to show how quickly program can get out of hand,
  // when you fail to look after the little things. The program finds the area under the
  // parabola y = x*x between x=0 and x=1, using a trapezoidal rule and several step sizes:

  def trapz() = {
    val mssg1 = "Area under the curve"
    val mssg2 = "by the trapezoidal rule"
    val mssg3 = "For delta x = 1/"

    var i: Double = 0.00
    var l: Double = 0.00
    var m: Double = 0.00
    var n: Int = 0

    var area1: Double = 0.00
    var area: Double = 0.00
    var lmts: Double = 0.00

      Console.println(mssg1)
      Console.println(mssg2)

    area = 0.00
           for(k <- 4 to 10) {
             m = 1.00 / k
             n = k - 1
      lmts = 0.50 * m

             i = 1.00

           for(j <- 1 to n) {
             l = math.pow(i / k, 2.00)
    area1 = 0.50 * m * (2.00 * l)
    area = area + area1
             if(i == n) out(k)
             else i = i + 1
      }
    }

    def out(k: Int) = {
      area = area + lmts
      Console.println(mssg3 + k + " " + area)
      area = 0.00
    }
  }

  // Held at arm's length, this program looks pretty impressive.
  // The is a large assortment of data declarations, followed by a computation
  // that is evidently complex enough to warrant a sub-procedure `out`.
  // Declarations are neatly aligned, and the executable statements are staggered
  // so as to indicate several levels of control nesting. There are text strings
  // to suggest the intent of the program, and mnemonic identifiers
  // to give hints about how the results are obtained. The general impression
  // conveyed is that this is a moderately complicated problem
  // that has been carefully coded and is now well under control.

  // Closer inspection, however, shows quite the opposite.

  // Each output message is used only once, and would be better placed in the
  // `Console.println` call itself. The first to messages can be even combined.
  // The purpose of the assignment `m = 1.00 / k` is unclear. Does it defend
  // against some mysterious conversion? Is it to convey geometrical insight?
  // Or does the programmer worry that computers divide more slowly than they multiply?
  // Efficiency cannot be of grave importance anyway, not when the code contains
  //              area1 = 0.50 * m * (2.00 * l)
  // We can now remove all variable declarations, so it is easier to see the underlying structure.
  // Variable `i`, it is labouriously kept equal to `j` so that `out` could be called
  // at the end of the last iteration. Clearly, `out` could be used just after inner loop is terminated,
  // it is better to inline it though, as it is used just once. The structure simplifies remarkably.
  // Now we can see that the summing variable `area` is supposed to be initialized
  // at the beginning of each loop on `k`, it is much better practice than clearing it
  // at the end of each iteration, especially from a remote procedure.
  // This procedure `out` is changing `area` and reading 'lmts', destroying modularity referring to
  // seamingly local variables in unexpected places, is an invitation to future bugs.

  // Putting all our improvements together gives,

  def shortTrapz() {
    Console.println("Area under the curve\nby the trapezoidal rule ")

    var area: Double = 0.00

    for(k <- 4 to 10) {
      area = 0.5 / k

      for(j <- 1 to (k-1)) {
        area = area + math.pow(j.toDouble / k, 2.00) / k
      }

      Console.println(s"For delta x = 1/$k $area")
    }
  }

  // The program now reflects how straightforward the calculation really is.
  // The changes we made were not designed to decrease execution time or to decrease storage utilization.
  // What then did we improve? Readability, principally, but also locality and simplicity of structure.

  // This will be the goal of all out revisions.
}
