package tutorial.webapp

import scala.scalajs.js.JSApp

object TutorialApp extends JSApp {
  case class Interval(length: Int, max: Int) {
    def add(value: Int) = Interval(length + 1, math.max(max, value))
  }
  private val emptyInterval = Interval(0, Int.MinValue)

  private def calcNextState(intervals: List[Interval], optValue: Option[Int]) = {
    val currentInterval = intervals.head
    (currentInterval, optValue) match {
      case (`emptyInterval`, None) => intervals
      case (_, None) => emptyInterval :: intervals
      case (_, Some(value)) => currentInterval.add(value) :: intervals.tail
    }
  }

  def calcIntervals(values: List[Option[Int]]): List[Interval] = {
    val initState = List(emptyInterval)
    val intervals = values.foldLeft(initState)(calcNextState)
    if (intervals.head == emptyInterval) intervals.tail.reverse
    else intervals.reverse
  }

  def main(): Unit = {
    val X = List(None, None, Some(-2), Some(-4), None,
      Some(-1), Some(3), Some(1), None, None,
      Some(2), Some(-4), Some(5), None)

    val intervalsLengthAndMax = calcIntervals(X)
    println(s"Result 2: ($intervalsLengthAndMax)")
  }
}
