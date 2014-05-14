import scala.chap12Applicative.SourceExercises.Applicative._

val depts: Map[String, String] = Map("Arun" -> "Chem", "Vickram" -> "Mech", "Pgn" -> "Naval")

val salaries: Map[String, Double] = Map("Arun" -> 12.0, "Vickram" -> 15.5, "Pgn" -> 17.5)

val o: Option[String] = optionApplicative.map2(depts.get("Arun"), salaries.get("Arun"))(
  (dept, salary) => s"Arun in $dept makes $salary"
)
