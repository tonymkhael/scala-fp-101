package murex.xva

import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.prop.{Checkers, GeneratorDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class DiffusionTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with Checkers {

  "Diffusion" should "be mapped" in {
    val data = DiffusionMatrix(DateSchedule(List(2l, 4l)), Seq(
      Seq(1d, 2d),
      Seq(3d, 4d)
    ))
    val doubled = data.map(x => {
      x * 2
    })
    val filtered = data upUntil 3l

    println(data)
    println(doubled)
    println(filtered)

    data.sum shouldBe 10
    data.size shouldBe(2, 2)

    doubled.sum shouldBe 20
    doubled.size shouldBe(2, 2)

    filtered.sum shouldBe 3
    filtered.size shouldBe(1, 2)

    val mod = data(2l, 0) = 2
    val modDoubled = mod.map(_ * 2)
    val modFiltered = mod upUntil 3

    mod.sum shouldBe 11
    modDoubled.sum shouldBe 22
    modFiltered.sum shouldBe 4

  }

  val genSchedule = Gen.nonEmptyContainerOf[List, Long](Gen.choose(1l, 5l)).map(DateSchedule(_))

  def matrixOf(rows: Int, cols: Int, min: Double, max: Double): Gen[Seq[Seq[Double]]] = {
    Gen.listOfN(rows, Gen.listOfN(cols, Gen.choose(min, max)))
  }

  val genDiffusion = for {
    s <- genSchedule
    m <- matrixOf(s.dates.length, 5, 1d, 5d)
  } yield DiffusionMatrix(s, m)

  "something" should "be something else" in {
    forAll(genDiffusion) { (matrix: DiffusionMatrix) =>
      matrix.size._2 should be (5)
      matrix.avg should (be >= 1d and be <= 5d)
    }
  }

}
