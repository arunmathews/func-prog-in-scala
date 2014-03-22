/**
 *
 */
import scala.chap4.Exercises._

map2(Some(5), Some("a"))((x, y) => x + y)

map2(Some(5), None: Option[String])((x, y) => x + y)

map3(Some(5), None: Option[Int])((x, y) => x + y)

map3(Some(5), Some("a"))((x, y) => x + y)

sequence(List(Some(5), None))

sequence(List(Some(5), Some(7)))

sequenceTraverse(List(Some(5), None))

sequenceTraverse(List(Some(5), Some(7)))
