package lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head.")
    case _ =>
  }
  /*
    - constants
    - wildcards
    - case classes
    - tuples
    - some special magic like above
   */

  // Not a case class!!
  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))
    def unapply(age: Int): Option[(String)] = Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n an I am $a years old."
    case _ =>
  }
  println(greeting)
  /*
    - match finds an object Person with an unapply method
    - Does it expect a parameter of the same type as bob? Yes!
    - Is the expected return type an Option consistent with the pattern I'm looking for? Yes!
    - If I call unapply with bob as a parameter, do I get a non-empty return? Yes!
    - In that case, the pattern matches.
    - If the Option is empty, though, it will not match. It's also not guaranteed that the actual name and age of bob
    are the things that were returned.
    - The name of the object doesn't have to be the same as the name of the class you're looking for.
   */

  val legalstatus = bob.age match {
    case Person(status) => s"My legal status is $status."
    case _ =>
  }
  println(legalstatus)

  // PART 2
  case class Or[A, B](a: A, b: B) // Either
  val either = Or(2, "two")
  val asWeKnowIt = either match {
    case Or(number, string) => s"$number is written as $string"
  }
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }
  println(asWeKnowIt)
  println(humanDescription)

  // decomposing sequences
  val varang = numbers match {
    case List(1, _*) => "starting with 1"
  }

  // unapply sequence

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  // we can name it whatever we want
  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1 and 2"
    // because of the _* compiler will look for unapplySeq
    // (1, 2, _*) will match the sequence returned
    case _ => "Blah"
  }
  println(decomposed)

  // custom return types for unapply
  // isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }
  println(bob match {
    case PersonWrapper(n) => s"This person's name is $n."
    case _ => "an alien"
  })
  // unapply can return any type, as long as it has the methods isEmpty and get
}
