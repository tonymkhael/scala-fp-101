package murex.xva

case class DateSchedule(dates: Seq[Long]) {

}

class DiffusionMatrix private (private val schedule: DateSchedule, private val matrix: Seq[Seq[Double]]) {

  def update(date: Long, scenario: Int, value: Double): DiffusionMatrix = {
    val row = schedule.dates.indexOf(date)
    new DiffusionMatrix(schedule, matrix.updated(row, matrix(row).updated(scenario, value)))
  }

  def scale(factor: Double) : DiffusionMatrix = map(_ * factor)

  def map(f: Double => Double): DiffusionMatrix = new DiffusionMatrix(schedule, matrix.view.map(_ map f))

  def upUntil(date: Long): DiffusionMatrix = {
    val s = DateSchedule(schedule.dates.takeWhile(_ <= date))
    new DiffusionMatrix(s, matrix.view.take(s.dates.length))
  }

  def avg: Double = sum / {
    val s = size; s._1 * s._2
  }

  def sum: Double = matrix.foldLeft(0d)((s, r) => r.sum + s)

  def size: (Int, Int) = (schedule.dates.length, matrix.head.length)

  override def toString: String = s"[${matrix.map(_.mkString(", ")).mkString("\n")}]"

}

object DiffusionMatrix {

  def apply(schedule: DateSchedule, matrix: Seq[Seq[Double]]) : DiffusionMatrix = {
    require(schedule.dates.nonEmpty, "Cannot have empty schedule")
    require(matrix.length == schedule.dates.length, "Date schedule and data are not the same size")
    val scenarios = matrix.head.length
    require(scenarios > 0, "First row is empty")
    require(matrix.forall(_.length == scenarios), "Some dates have different scenario sizes")
    new DiffusionMatrix(schedule, matrix)
  }

}
