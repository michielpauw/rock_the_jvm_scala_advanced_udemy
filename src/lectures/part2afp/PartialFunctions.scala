package lectures.part2afp

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FuntionNotApplicableException

  class FuntionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // {1, 2, 5} => Int
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value

  println(aPartialFunction(2))
  // println(aPartialFunction(324)) => will crash with a MatchError

  // PF utilities
  println(aPartialFunction.isDefinedAt(67))
  // lift
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(4554))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(pfChain(45))

  // PF extend normal functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }
  // HOFs accept partial functions as well
  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  // Note: PF can only have ONE parameter type
}
