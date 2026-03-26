package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Person
import u03.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Person.teacherCourses

class PersonTest:

  val people: Sequence[Person] = Cons(Teacher("Viroli", "PPS"),
    Cons(Student("Mario", 2020),
      Cons(Teacher("Ricci", "PCD"), Nil())))

  @Test def testTeacherCourses() =
    assertEquals(
      Cons("PPS", Cons("PCD", Nil())),
      teacherCourses(people)
    )

  @Test def testDistinctCourseCount() =
    val teachers = Cons(Teacher("Viroli", "PPS"),
      Cons(Teacher("Aguzzi", "PPS"),
        Cons(Teacher("Ricci", "PCD"), Nil())))
    assertEquals(2, distinctCourseCount(teachers))