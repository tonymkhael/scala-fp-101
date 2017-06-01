package murex.xva

import murex.xva.Implicits.DoubleToDiffusion
import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class DiffusionTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with Checkers {

  "Diffusion" can "be created using a date and a scalar value" in {
    val diffusion = Diffusion(2, 1)

    println(diffusion)

    diffusion.dates shouldBe Seq(2)
    diffusion.sum shouldBe 1
    diffusion.size shouldBe(1, 1)
  }

  "Diffusion" can "be created using a scalar and implicit date of 1" in {
    val diffusion = Diffusion(1)

    println(diffusion)

    diffusion.dates shouldBe Seq(1)
    diffusion.sum shouldBe 1
    diffusion.size shouldBe(1, 1)
  }

  "Diffusion" can "be created using a date schedule and a 2d double matrix" in {
    val diffusion = Diffusion(DateSchedule(2l, 4l),
      Seq(1d, 2d),
      Seq(3d, 4d)
    )

    (Diffusion(2) + Diffusion(1)).sum shouldBe 3

    println(diffusion)

    diffusion.dates shouldBe Seq(2, 4)
    diffusion.sum shouldBe 10
    diffusion.size shouldBe(2, 2)
  }

  "Diffusion" can "be created with a 2d double matrix and an implicit linear schedule" in {
    val diffusion = Diffusion(Seq(
      Seq(1d, 2d),
      Seq(3d, 4d)
    ))

    println(diffusion)

    diffusion.dates shouldBe Seq(1, 2)
    diffusion.sum shouldBe 10
    diffusion.size shouldBe(2, 2)
  }


  "Diffusion" should "be mapped lazily" in {
    val diffusion = oneTwoThreeFour

    val doubled = diffusion.map(x => {
      x * 2
    })

    println(diffusion)
    println(doubled)

    diffusion.sum shouldBe 10
    diffusion.size shouldBe(2, 2)

    doubled.sum shouldBe 20
    doubled.size shouldBe(2, 2)
  }

  "Diffusion" should "be filtered lazily" in {
    val dm = oneTwoThreeFour
    val filtered = dm upUntil 3l

    println(dm)
    println(filtered)

    dm.sum shouldBe 10
    dm.size shouldBe(2, 2)

    filtered.sum shouldBe 3
    filtered.size shouldBe(1, 2)
  }

  "Diffusion" should "be lazily mapped and filtered" in {
    val diffusion = oneTwoThreeFour

    println(diffusion)

    val mod = diffusion(2l, 0) = 2
    val modDoubled = mod.map(_ * 2)
    val modFiltered = mod upUntil 3

    println(mod)
    println(modDoubled)
    println(modFiltered)

    mod.sum shouldBe 11
    modDoubled.sum shouldBe 22
    modFiltered.sum shouldBe 4

  }

  "Diffusion" should "be lazily modified" in {
    val diffusion = oneTwoThreeFour

    println(diffusion)

    val mod = diffusion(2l, 0) = 2
    val modDoubled = mod.map(_ * 2)
    val modFiltered = mod upUntil 3

    println(mod)
    println(modDoubled)
    println(modFiltered)

    mod.sum shouldBe 11
    modDoubled.sum shouldBe 22
    modFiltered.sum shouldBe 4

  }

  "Diffusion" should "be scaled using *" in {
    val diffusion = oneTwoThreeFour

    val doubled = diffusion * 2d

    println(diffusion)
    println(doubled)

    diffusion.sum shouldBe 10
    diffusion.size shouldBe(2, 2)

    doubled.sum shouldBe 20
    doubled.size shouldBe(2, 2)

  }

  "Diffusion" should "be modified using +" in {
    val diffusion = oneTwoThreeFour

    val plusOne = diffusion + 1d

    println(diffusion)
    println(plusOne)

    diffusion.sum shouldBe 10
    diffusion.size shouldBe(2, 2)

    plusOne.sum shouldBe 14
    plusOne.size shouldBe(2, 2)

  }

  "Diffusion" can "be used with + and * in any position using implicits" in {
    val diffusion = oneTwoThreeFour

    val plusOne = 1d + diffusion * 2

    println(diffusion)
    println(plusOne)

    diffusion.sum shouldBe 10
    diffusion.size shouldBe(2, 2)

    plusOne.sum shouldBe 24
    plusOne.size shouldBe(2, 2)

  }

  "Diffusion" can "be bu multiplied with another" in {
    val a = Diffusion(DateSchedule(2l, 4l),
      Seq(1d, 2d),
      Seq(3d, 4d)
    )

    val b = Diffusion(DateSchedule(2l, 4l),
      Seq(2d, 0d),
      Seq(1d, 2d)
    )

    val m = a * b

    println(m)

    m(2,0) shouldBe 4
    m(2,1) shouldBe 4
    m(4,0) shouldBe 10
    m(4,1) shouldBe 8

  }

  "diffusion average" should "stay between min and max" in {
    forAll(genDiffusion(5, 5, 1d, 5d)) { (diffusion: Diffusion) =>
      println(diffusion)
      diffusion.avg should (be >= 1d and be <= 5d)
    }
  }

  case class Trade(id: String, maturity: Long, payoff: Double) {

  }

  "Diffusion" can "be used to evaluate a trade" in {

    val trade = Trade("1", 2, 5.0)

    val diffusion = oneTwoThreeFour

    val mv = (diffusion upUntil trade.maturity) * trade.payoff

    println(mv)

    mv(2, 0) shouldBe 5
    mv(2, 1) shouldBe 10
  }

  private def newSchedule(count: Int): Gen[DateSchedule] = {
    Gen.const(DateSchedule(1l to count.toLong: _*))
  }

  private def matrixOf(maxRows: Int, maxCols: Int, min: Double, max: Double): Gen[Seq[Seq[Double]]] = {
    val c = Gen.choose(1, maxCols).sample
    Gen.nonEmptyListOf(Gen.listOfN(c.get, Gen.choose(min, max)))
  }

  private def genDiffusion(maxDates: Int, maxScenarios: Int, min: Double, max: Double): Gen[Diffusion] = for {
    m <- matrixOf(maxDates, maxScenarios, min, max)
    s <- newSchedule(m.size)
  } yield Diffusion(s, m: _*)

  private def oneTwoThreeFour: Diffusion = {
    Diffusion(DateSchedule(2l, 4l),
      Seq(1d, 2d),
      Seq(3d, 4d)
    )
  }

}
