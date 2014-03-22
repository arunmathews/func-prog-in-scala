/**
 *
 */

import scala.chap4.Chapter._
val someOp:Option[Int] = Some(5)
val noneOp:Option[Int] = None
someOp map(a => a*2)
noneOp map(a => a*2)
someOp orElse(Some(-1))
noneOp orElse(Some(22))
someOp flatMap(a => Some(a))
noneOp flatMap(a => Some(a))

case class Employee(name: String, age: Int, salary: Double)

for {
  age <- Right(42)
  name <- Left("invalid name")
  salary <- Right(1000000.0)
} yield Employee(name, age, salary)


val test = Right(42)

val testFn = (a: Int) => if(a > 0) Right(a) else Left("Negative number")


test.map(testFn)

for {
  x <- Right(42)
  y <- testFn(x)
} yield y


Right(-25).map(testFn)

//flatMap
for {
  x <- Right(-42)
  y <- testFn(x)
} yield y

//map
for {
  x <- Right(-42)
} yield testFn(x)

sealed class Name(val value: String)
sealed class Age(val value: Int)
case class Person(name: Name, age: Age)
def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))
def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))
def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

mkPerson("ok", 5)


mkPerson(null, 5)

mkPerson("notOk", -7)

mkPerson(null, -7)

def mkNameMult(name: String): Partial[String, Name] =
  if (name == "" || name == null) Errors(Seq("Name is empty."))
  else Success(new Name(name))
def mkAgeMult(age: Int): Partial[String, Age] =
  if (age < 0) Errors(Seq("Age is out of range."))
  else Success(new Age(age))

mkNameMult("ok")
mkAgeMult(5)

mkNameMult("ok").map(mkAgeMult(5))
def mkPersonMult(name: String, age: Int): Partial[String, Person] =
  mkNameMult(name).map(mkAgeMult(age))(Person(_, _))





