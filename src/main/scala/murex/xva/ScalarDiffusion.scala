package murex.xva

class ScalarDiffusion private[murex] (private val date: Long, private val x: Double) extends Diffusion {

  override def update(date: Long, scenario: Int, value: Double): Diffusion = {
    require(date == this.date, "Operation will lead to empty diffusion")
    new ScalarDiffusion(date, value)
  }

  override def dates: Seq[Long] = Seq(date)

  override def map(f: (Double) => Double): Diffusion = new ScalarDiffusion(date, f(x))

  override def mapWithIndex(f: (Long, Int, Double) => Double): Diffusion = new ScalarDiffusion(date, f(date, 0, x))

  override def upUntil(date: Long): Diffusion = {
    require(date >= this.date, "Operation will lead to empty diffusion")
    this
  }

  override def apply(date: Long, scenario: Int): Double = {
    require(date == this.date, s"No value for date $date")
    x
  }

  override def avg: Double = x

  override def sum: Double = x

  override def size: (Int, Int) = (1, 1)

  override def toString: String = s"| $date | -> Schedule\n| $x |"

}
