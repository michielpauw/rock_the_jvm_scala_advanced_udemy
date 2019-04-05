package lectures.part2afp

object CurriesAndPAF extends App {

  // curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) //Int => Int = y => 3 + y

  println(add3(5))
  println(superAdder(3)(5))

  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method
  val add4: Int => Int = curriedAdder(4)
  // val add4 = curriedAdder(4) // won't work!
  // because curried adder is a method, you need to pass all parameter lists
  // now we converted a method into a function value of Int => Int
  // lifting or ETA-EXPANSION
  // used to create functions out of methods.
  // functions != methods (JVM limitations)

  def inc(x: Int) = x + 1
  List(1, 2, 3).map(inc) // ETA-expansion turns method into function
  List(1, 2, 3).map(x => inc(x))

  // Partial function applications
  val add5 = curriedAdder(5) _ // will tell compiler to do an ETA-expansion and turn it into an Int => Int

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  // x: String => concatenator("Hello, I'm ", x, ", how are you?")
  println(insertName("Michiel"))
  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello, ", x, y)
  println(fillInTheBlanks("Michiel", ", Scala is awesome."))
}
