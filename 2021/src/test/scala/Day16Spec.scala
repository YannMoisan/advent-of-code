import Day16._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Spec extends AnyFlatSpec with Matchers {
  behavior of "Part 1"

  it should "work on example 1" in {
    sum(parse(toBinary("8A004A801A8002F478"))._1) shouldEqual 16
  }

  it should "work on example 2" in {
    sum(parse(toBinary("620080001611562C8802118E34"))._1) shouldEqual 12
  }

  it should "work on example 3" in {
    sum(parse(toBinary("C0015000016115A2E0802F182340"))._1) shouldEqual 23
  }

  it should "work on example 4" in {
    sum(parse(toBinary("A0016C880162017C3686B18A3D4780"))._1) shouldEqual 31
  }

  behavior of "Part 2"

  it should "C200B40A82 finds the sum of 1 and 2" in {
    compute(parse(toBinary("C200B40A82"))._1) shouldEqual 3L
  }

  it should "04005AC33890 finds the product of 6 and 9" in {
    compute(parse(toBinary("04005AC33890"))._1) shouldEqual 54L
  }

  it should "880086C3E88112 finds the minimum of 7, 8, and 9" in {
    compute(parse(toBinary("880086C3E88112"))._1) shouldEqual 7L
  }

  it should "CE00C43D881120 finds the maximum of 7, 8, and 9" in {
    compute(parse(toBinary("CE00C43D881120"))._1) shouldEqual 9L
  }

  it should "D8005AC2A8F0 produces 1, because 5 is less than 15" in {
    compute(parse(toBinary("D8005AC2A8F0"))._1) shouldEqual 1L
  }

  it should "F600BC2D8F produces 0, because 5 is not greater than 15." in {
    compute(parse(toBinary("F600BC2D8F"))._1) shouldEqual 0L
  }

  it should "9C005AC2F8F0 produces 0, because 5 is not equal to 15." in {
    compute(parse(toBinary("9C005AC2F8F0"))._1) shouldEqual 0L
  }

  it should "9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2" in {
    compute(parse(toBinary("9C0141080250320F1802104A08"))._1) shouldEqual 1L
  }
}
