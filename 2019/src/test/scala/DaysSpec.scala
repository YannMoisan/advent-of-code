import org.scalatest.{FlatSpec, Matchers}

class DaysSpec extends FlatSpec with Matchers {
  "Day 1" should "be correct" in {
    Puzzles.runPart1(Day1) shouldBe 3457281
    Puzzles.runPart2(Day1) shouldBe 5183030
  }

  "Day 2" should "be correct" in {
    Puzzles.runPart1(Day2) shouldBe 3765464
    Puzzles.runPart2(Day2) shouldBe 7610
  }

  "Day 3" should "be correct" in {
    Puzzles.runPart1(Day3) shouldBe 399
    Puzzles.runPart2(Day3) shouldBe 15678
  }

  "Day 4" should "be correct" in {
    Puzzles.runPart1(Day4) shouldBe 1864
    Puzzles.runPart2(Day4) shouldBe 1258
  }

  "Day 5" should "be correct" in {
    Puzzles.runPart1(Day5) shouldBe 7265618
    Puzzles.runPart2(Day5) shouldBe 7731427
  }

  "Day 6" should "be correct" in {
    Puzzles.runPart1(Day6) shouldBe 278744
    Puzzles.runPart2(Day6) shouldBe 475
  }

  "Day 7" should "be correct" in {
    Puzzles.runPart1(Day7) shouldBe 21860
    Puzzles.runPart2(Day7) shouldBe 2645740
  }

  "Day 8" should "be correct" in {
    Puzzles.runPart1(Day8) shouldBe 2975
    Puzzles.runPart2(Day8) shouldBe
      """**** *  * ***  *  * **** 
        |*    *  * *  * *  * *    
        |***  **** *  * *  * ***  
        |*    *  * ***  *  * *    
        |*    *  * * *  *  * *    
        |**** *  * *  *  **  **** """.stripMargin
  }

  "Day 9" should "be correct" in {
    Puzzles.runPart1(Day9) shouldBe 3063082071L
    Puzzles.runPart2(Day9) shouldBe 81348
  }

}
