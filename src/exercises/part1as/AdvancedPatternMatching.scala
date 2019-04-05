package exercises.part1as

object AdvancedPatternMatching extends App {

  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0) Some(true)
      else None
  }

  // single Boolean test: you don't need options
  object singleDigit {
    def unapply(arg: Int): Boolean = (arg > -10 && arg < 10)
  }

  val n: Int = 4
  val mathProperty = n match {
    case singleDigit() => "single digit"
    case even(_) => "an even number"
    case _ => "no property"
  }
  println(mathProperty)
}
