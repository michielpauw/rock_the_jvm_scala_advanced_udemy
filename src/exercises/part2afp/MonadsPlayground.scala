package exercises.part2afp

class Lazy[+A](value: => A) {
  private lazy val internalValue = value
  def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
}

object Lazy {
  def apply[A](a: => A): Lazy[A] = new Lazy(a)
}