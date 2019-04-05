package lectures.part2afp

object LazyEvaluation extends App {

  // lazy delays evaluation of values. x is only evaluated when it's used
  lazy val y: Int = throw new RuntimeException
  // the following line will make the program crash
  // println(y)

  // it only gets evaluated once, though!
  lazy val x: Int = {
    println("hello")
    42
  }
  println(x)
  println(x)

  // examples of implications
  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition

  println(if (simpleCondition && lazyCondition) "yes" else "no")
  // side effect is not printed, because sideEffectCondition is not evaluated, because simpleCondition is false already
  println(if (!simpleCondition && !lazyCondition) "yes" else "no")

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = n + n + n + 1
  def retrieveMagicValue = {
    // side effect or a long computation
    println("Waiting")
    Thread.sleep(1000)
    42
  }
  // n is evaluated three times and waits for three seconds!
  // println(byNameMethod(retrieveMagicValue))
  // use lazy vals!
  def byNameMethodBetter(n: => Int): Int = {
    // CALL BY NEED
    lazy val t = n
    t + t + t + 1
  }
  // println(byNameMethodBetter(retrieveMagicValue))

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }
  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30) // List(1, 25, 5, 23)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30) // lazy values under the hood
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println
  println(gt20lazy)
  // filter will only get evaluated when needed! Pay attention to the order.
  println(gt20lazy.foreach(println))

  // for-comprehensions use withFilter with guards
  for {
    a <- List(1, 2, 3) if a % 2 == 0 // use lazy vals!
  } yield a + 1

  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1) // List[Int]

  def whileLoop(condition: => Boolean)(body: => Unit): Unit =
    if (condition) {
      body
      whileLoop(condition)(body)
    }

  var i = 2

  whileLoop (i > 0) {
    println(i)
    i -= 1
  }  // prints 2 1
}
