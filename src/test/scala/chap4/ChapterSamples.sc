/**
 *
 */

import scala.chap4.ChapterSamples._

val employeesByName: Map[String, Employee] =
  List(Employee("Alice", "R&D"), Employee("Bob", "Accounting")).
    map(e => (e.name, e)).toMap


val dept: Option[String] = employeesByName.get("Joe").map(_.dept)

doesMatch("^a.*$", "abc")

doesMatch("^a.*$\\\\\\", "abc")