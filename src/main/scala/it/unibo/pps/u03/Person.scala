package u03

import u03.Person
import u03.Person.*
import u03.Sequences.*
import u03.Sequences.Sequence.*


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
    foldLeft(distinct(teacherCourses(ps)))(0)()