package u03

import u03.Optionals.Optional
import Sequences.*
import Sequences.Sequence.*

object Sequences:

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) if n == 0 => s
      case Cons(h, t) => skip(t)(n - 1)

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _) => Nil()
      case (_, Nil()) => Nil()
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1,h2), zip(t1,t2))

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Nil() => s2
      case Cons(h,t) => Cons(h, concat(t, s2))

    def reverse[A](s: Sequence[A]): Sequence[A] =
      def loop(rem: Sequence[A], acc: Sequence[A]): Sequence[A] = rem match
        case Nil() => acc
        case Cons(h, t) => loop(t, Cons(h, acc))
      loop(s, Nil())

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Nil() => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    def min(s: Sequence[Int]): Optional[Int] = s match
      case Nil() => Optional.Empty()
      case Cons(h, Nil()) => Optional.Just(h)
      case Cons(h, t) => min(t) match
        case Optional.Just(min) => Optional.Just(math.min(h, min))
        case Optional.Empty() => Optional.Just(h)

    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, Nil()) => Cons(h, Nil())
      case Cons(h, Cons(_, t)) => Cons(h, evenIndices(t))

    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Nil() => false
      case Cons(h, t) if h == elem => true
      case Cons(_, t) => contains(t)(elem)

    def distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))

    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match
      case Nil() => Nil()
      case Cons(first, tail) => group(tail) match
        case Nil() =>
          Cons(Cons(first, Nil()), Nil())
        case Cons(firstGroup, others) => firstGroup match
          case Cons(firstElement, firstGroupTail) if first == firstElement =>
            Cons(Cons(first, firstGroup), others)
          case _ =>
            Cons(Cons(first, Nil()), Cons(firstGroup, others))

    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = s match
      case Nil() => (Nil(), Nil())
      case Cons(h, t) => partition(t)(pred) match
        case (satisfied, notSatisfied) =>
          if pred(h) then (Cons(h, satisfied), notSatisfied)
          else (satisfied, Cons(h, notSatisfied))

    def foldLeft[A, B](s: Sequence[A])(default: B)(op: (B, A) => B): B = s match
      case Nil() => default
      case Cons(h, t) => foldLeft(t)(op(default,h))(op)

// Task 2



enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  def name(p: Person): String = p match
    case Student(n, _) => n
    case Teacher(n, _) => n

  def teacherCourses(ps: Sequence[Person]): Sequence[String] = flatMap(ps):
    case Person.Teacher(_, course) => Cons(course, Nil())
    case _ => Nil()

  def distinctCourseCount(ps: Sequence[Person]): Int =
    foldLeft(distinct(teacherCourses(ps)))(0)((acc, _) => acc + 1)

// Task 3



object Streams:

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case Cons(head, tail) => Empty()
      case _ => Empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case 0 => Empty()
      case _ => cons(k, fill(n-1)(k))

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    val fibonacci: Stream[Int] =
      def fib(a: Int, b: Int): Stream[Int] = cons(a, fib(b, a + b))
      fib(0, 1)
