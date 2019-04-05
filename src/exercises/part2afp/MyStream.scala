package exercises.part2afp

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concatenate two streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of this stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

/*
  naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!/**/
  naturals.take(100) // lazily evaluated stream of the first 100 naturals (finite stream)
  naturals.take(100).foreach(println) // should run fine
  naturals.foreach(println) //will crash!
  naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
*/

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new Cons(start, MyStream.from(generator(start))(generator))
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element, this)
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this
}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false
  override val head: A = hd
  override lazy val tail: MyStream[A] = tl // CALL BY NEED (combination of call by name param and lazy val in implem.)

  /*
    val s = new Cons(1, EmptyStream)
    val prepended = 1 #:: s = new Cons(1, s) ==> tail will still be unevaluated
   */
  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)
  // ++ Still preserves lazy evaluation in the stream!
  // We do need to call it BY NAME!! Otherwise it will evaluate flatMap before concat.
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  /*
    s = new Cons(1, ???)
    mapped = s.map(_ + 1) = new Cons(2, s.tail.map(_ + 1))
    s.tail.map(_ + 1) is not evaluated until I call mapped.tail
   */
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate) // evaluates the current tail, but not the remaining tails. Preserves lazy evaluation!

  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream) // optimization
    else new Cons(head, tail.take(n - 1)) // still preserves lazy evaluation
}

object StreamsPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.tail.head)
  val startFrom0 = 0 #:: naturals
  println(startFrom0.head)
  startFrom0.take(10000).foreach(println)

  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(100).toList())
  val veryNatural = naturals ++ startFrom0
  println(veryNatural.take(100).toList())
  println(startFrom0.filter(_ < 10).take(10).toList())
  println(startFrom0.filter(_ < 10).take(10).take(20).toList())

  val fibonacci_diff = MyStream.from((0, 1))(x => (x._2, x._1 + x._2)).flatMap(x => new Cons(x._1, EmptyStream))
  def fibonacci(first: Int, second: Int): MyStream[Int] =
    new Cons(first, fibonacci(second, first + second))
  println(fibonacci(0, 1).takeAsList(100))

  lazy val naturalNumbers = MyStream.from(2)(_ + 1)
  lazy val naturalNumbers_2 = MyStream.from(2)(_ + 1)

  def primeNumbers(t: MyStream[Int]): MyStream[Int] = {
    if (t.isEmpty) t
    else new Cons(t.head, primeNumbers(t.filter(_ % t.head != 0)))
  }

  println(primeNumbers(naturalNumbers).takeAsList(100))
}