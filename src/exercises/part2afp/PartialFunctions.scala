package exercises.part2afp

object PartialFunctions extends App {

  val aPartialFunction = new {
    def apply(value: Int): Int = {
      val isMatch = matches(value)
      if (isMatch.isEmpty) throw new MatchError
      else isMatch.get
    }

    def matches(value: Int): Option[Int] = {
      value match {
        case 1 => Some(42)
        case 2 => Some(56)
        case 5 => Some(999)
        case _ => None
      }
    }

    def isDefinedAt(value: Int): Boolean = {
      if (matches(value).isEmpty) false
      else true
    }

    def lift: Int => Option[Int] = {
      matches
    }
  }

  println(aPartialFunction(2))

  val chatbot: PartialFunction[String, Unit] = {
    case "Hi" => println("Hello")
    case "Hello" => println("Bonjour")
    case "How are you?" => println("I'm fine, thanks!")
    case _ => println("I'm sorry, I don't quite understand...")
  }
  scala.io.Source.stdin.getLines().foreach(line => chatbot(line))
}
