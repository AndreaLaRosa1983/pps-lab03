package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import u03.Sequences.*
import u03.Sequences.Sequence.*

class StreamTest:
  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_ + 1)
    assertEquals(
      Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))),
      Stream.toList(Stream.takeWhile(stream)(_ < 5))
    )
    assertEquals(
      Nil(),
      Stream.toList(Stream.takeWhile(stream)(_ < 0))
    )
    assertEquals(
      Cons(0, Nil()),
      Stream.toList(Stream.takeWhile(stream)(_ < 1))
    )

  @Test def testFill() =
    assertEquals(
      Cons("a", Cons("a", Cons("a", Nil()))),
      Stream.toList(Stream.fill(3)("a"))
    )
    assertEquals(
      Nil(),
      Stream.toList(Stream.fill(0)("a"))
    )

  @Test def testFibonacci() =
    assertEquals(
      Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))),
      Stream.toList(Stream.take(Stream.fibonacci)(5))
    )



