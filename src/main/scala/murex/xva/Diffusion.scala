package murex.xva

trait Diffusion {

  def +(other: Diffusion): Diffusion = {
    require(size == other.size, "Diffusions are not of same size")
    require(dates == other.dates, "Date schedule does not match")
    other match {
      case ScalarDiffusion(x) => this + x
      case diffusion => this mapWithIndex  { (date, scenario, v) => v + diffusion(date, scenario)
      }
    }
  }

  def *(other: Diffusion): Diffusion = {
    require(size._2 == other.size._1, "Diffusions cannot be multiplied")
    require(dates == other.dates, "Date schedule does not match")
    val rows = Seq.newBuilder[Seq[Double]]
    var i = 0
    while(i < size._1){
      var j = 0
      val cols = Seq.newBuilder[Double]
      while(j < size._2){
        var r = 0d
        var k = 0
        while(k < size._2){
          r += this(dates(i), k) * other(dates(k), j)
          k = k +1
        }
        cols += r
        j = j+1
      }
      rows += cols.result
      i= i +1
    }
    Diffusion(DateSchedule(this.dates : _*), rows.result : _*)
  }

  def +(d: Double): Diffusion = this map (_ + d)

  def *(d: Double): Diffusion = this map (_ * d)

  def update(date: Long, scenario: Int, value: Double): Diffusion

  def dates: Seq[Long]

  final def scale(factor: Double): Diffusion = map(_ * factor)

  def map(f: Double => Double): Diffusion

  def mapWithIndex(f: (Long, Int, Double) => Double) : Diffusion

  def upUntil(date: Long): Diffusion

  def avg: Double = sum / {
    val s = size
    s._1 * s._2
  }

  def sum: Double

  def size: (Int, Int)

  def apply(date: Long, scenario: Int): Double

  object ScalarDiffusion {
    def unapply(diffusion: Diffusion): Option[Double] = {
      if (diffusion.size == (1, 1))
        Some(diffusion.sum)
      else
        None
    }
  }

}

object Implicits {

  implicit class DoubleToDiffusion(val d: Double) extends AnyVal {

    def +(m: Diffusion): Diffusion = m map (_ + d)

    def *(m: Diffusion): Diffusion = m map (_ * d)

  }

}

object Diffusion {

  def apply(schedule: DateSchedule, matrix: Seq[Double]*): Diffusion = {
    val dates = schedule.dates
    require(dates.nonEmpty, "Cannot have empty schedule")
    require(matrix.length == dates.length, "Date schedule and data are not the same size")

    val firstRow = matrix.head
    val scenarios = firstRow.length
    require(scenarios > 0, "First row is empty")
    require(matrix.forall(_.length == scenarios), "Some dates have different scenario sizes")

    (dates.length, scenarios) match {
      case (1, 1) => new ScalarDiffusion(dates.head, firstRow.head)
      case _ => new DiffusionMatrix(schedule, matrix)
    }

  }

  def apply(matrix: Seq[Seq[Double]]): Diffusion = {
    apply(DateSchedule(1l to matrix.length: _*), matrix: _*)
  }

  def apply(date: Long, value: Double): Diffusion = new ScalarDiffusion(date, value)

  def apply(value: Double): Diffusion = apply(1l, value)

}
