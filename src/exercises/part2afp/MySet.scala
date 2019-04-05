package exercises.part2afp

trait MySet[A] extends (A => Boolean) {

  def apply(elem: A): Boolean =
    contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def -(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def -(elem: A): MySet[A] = this
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def --(anotherSet: MySet[A]) = this
  def &(anotherSet: MySet[A]) = this

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => true)
}

// all elements of type A which satisfy a property.
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  def contains(elem: A): Boolean = property(elem)
  def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)
  def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => anotherSet(x) || property(x))
  def -(elem: A): MySet[A] = filter(x => x != elem)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def foreach(f: A => Unit): Unit = politelyFail
  def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => predicate(x) && property(x)) // property based set
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)
  def +(elem: A): MySet[A] =
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)
  def -(elem: A): MySet[A] =
    if (head != elem) (tail - elem) + head
    else tail
  def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head
  def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // => refactored from filter(x => anotherSet.contains(x))
  // intersection = filtering

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ (tail flatMap f)
  def filter(predicate: A => Boolean): MySet[A] =
    if (predicate(head)) (tail filter predicate) + head
    else tail filter predicate
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {

  val s = MySet(1, 2, 3, 4)
  s foreach println
  MySet(-1, -2, 4) + 3 -- s foreach println

}