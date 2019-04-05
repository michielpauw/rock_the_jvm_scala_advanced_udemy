package exercises.part2afp

import scala.util.Random

object CurriesAndPAF extends App {

  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)( y: Int) = x + y

  def add7_conv(f: (Int, Int) => Int): (Int => Int) = x => f(7, x)
  def add7_1 = add7_conv(simpleAddFunction)
  val add7_2: Int => Int = curriedAddMethod(7)
  val add7_3 = curriedAddMethod(7) _
  val add7_4 = (x: Int) => simpleAddFunction(7, x)
  val add7_5 = (x: Int) => simpleAddMethod(7, x)
  val add7_6 = (x: Int) => curriedAddMethod(7)(x)
  val add7_7 = simpleAddFunction.curried(7)
  val add7_8 = curriedAddMethod(7)(_)
  val add7_9 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
                // y => simpleAddMethod(7, y)
  val add7_10 = simpleAddFunction(7, _: Int)

//  val curriedFormatter = (s: String, x: Double) => s.format(x)
//  val leastAccurate = curriedFormatter("%4.2f", _: Double)
//  val mediumAccurate = curriedFormatter("%8.6f", _: Double)
//  val mostAccurate = curriedFormatter("%14.12f", _: Double)

  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val leastAccurate = curriedFormatter("%4.2f") _
  val mediumAccurate = curriedFormatter("%8.6f") _
  val mostAccurate = curriedFormatter("%14.12f") _

  val listOfFloats = Seq.fill(10)(Random.nextDouble() * Random.nextInt())
  println(listOfFloats.map(leastAccurate(_)))
  println(listOfFloats.map(mediumAccurate(_)))
  println(listOfFloats.map(mostAccurate(_)))

  // NOTE: no underscore needed, because compiler does ETA expansion for us:
  println(listOfFloats.map(curriedFormatter("%14.12f")))

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1
  def method: Int = 42
  def parenMethod(): Int = 42

  val lambda = () => 42
  val paf = parenMethod _

  println(byName(42))
  println(byName(method))
  println(byName(parenMethod())) // byName calls method and actually uses that value, not the function itself.
  println(byName(parenMethod))// ok but beware ==> byName(parenMethod())
  //  println(byName(lambda)) // not ok!
  println(byName(lambda())) // ok if you call it!
  //  println(byName(paf)) // not ok!
  println(byName(paf())) // once again...

  //  println(byFunction(42))
  //  println(byFunction(method)) // NOT OK!!!!!! Compiler does not do ETA-expansion
  println(byFunction(method _))
  //  println(byFunction(parenMethod()))
  //  println(byFunction(parenMethod() _))
  println(byFunction(parenMethod)) // Compiler DOES do ETA-expansion for us
  println(byFunction(parenMethod _)) // also works, but _ is unnecessary ^^, so warning generated
  println(byFunction(lambda))
  //  println(byFunction(lambda()))
  println(byFunction(paf)) // same as parenMethod _
  //  println(byFunction(paf()))
}
