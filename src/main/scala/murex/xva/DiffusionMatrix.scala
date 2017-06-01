package murex.xva

class DiffusionMatrix private[murex](private val schedule: DateSchedule, private val matrix: Seq[Seq[Double]]) extends Diffusion {

  override def update(date: Long, scenario: Int, value: Double): Diffusion = {
    val row = schedule.dates.indexOf(date)
    Diffusion(schedule, matrix.updated(row, matrix(row).updated(scenario, value)): _*)
  }

  override def apply(date: Long, scenario: Int): Double = {
    val row = schedule.dates.indexOf(date)
    matrix(row)(scenario)
  }

  override def dates: Seq[Long] = schedule.dates

  override def map(f: Double => Double): DiffusionMatrix = new DiffusionMatrix(schedule, matrix.view.map(_ map f))

  override def mapWithIndex(f: (Long, Int, Double) => Double): Diffusion = {
    new DiffusionMatrix(schedule, matrix.view.zipWithIndex.map {
      case (vs, row) => vs.zipWithIndex.map {
        case (v, scenario) => f(schedule.dates(row), scenario, v)
      }
    })
  }

  override def upUntil(date: Long): Diffusion = {
    val s = DateSchedule(schedule.dates.takeWhile(_ <= date): _ *)
    Diffusion(s, matrix.view.take(s.dates.length): _*)
  }

  override def sum: Double = matrix.foldLeft(0d)((s, r) => r.sum + s)

  override def size: (Int, Int) = (schedule.dates.length, matrix.head.length)

  override def toString: String = s"| ${schedule.dates.mkString(", ")} | -> Schedule\n| ${matrix.map(_.mkString(", ")).mkString(" |\n| ")} |"

}
