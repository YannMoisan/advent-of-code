object Day7 extends MultiPuzzle[String, String] {
  override def part1(iter: Iterator[String]) : String = {
    val lines = iter.toArray
    val pattern = """Step (\w) must be finished before step (\w) can begin.""".r
    var instructions: Array[(String, String)] = lines.map{ case pattern(a,b ) => (a, b)}
    //parsed.take(5).foreach(println)

    var solutions = Seq.empty[String]

    while (!instructions.isEmpty) {
      val allSteps = instructions.flatMap{case (a, b) => Seq(a,b)}.distinct
      //println(allSteps.mkString(","))
      val canBegin = allSteps.filterNot(step => instructions.map(_._2).contains(step)).sorted.head
      println("canBegin:"+canBegin.mkString(","))
      solutions = solutions :+ canBegin
      instructions = instructions.filterNot(i => canBegin == i._1)
      println("instr:"+instructions.mkString(","))
    }


    solutions.mkString
  }

  override def part2(iter: Iterator[String]) : String = {
    println("--- paRT2 ---")
    val lines = iter.toArray
    val pattern = """Step (\w) must be finished before step (\w) can begin.""".r
    var instructions: Array[(String, String)] = lines.map{ case pattern(a,b ) => (a, b)}
    //parsed.take(5).foreach(println)

    var solutions = Seq.empty[String]
    var workers = Seq.empty[(String, Int)]
    var time = 0

    while (!instructions.isEmpty || !workers.isEmpty) {
      println("time:"+time)
      val done = workers.filterNot(w => time < w._2)
      println("done:"+done)
      instructions = instructions.filterNot(i => done.map(_._1).contains(i._1))
      workers = workers.filter(w => time < w._2)
      println("workers:" + workers.mkString(","))


      val allSteps = instructions.flatMap{case (a, b) => Seq(a,b)}.distinct
      println("allSteps"+allSteps.mkString(","))

      //solutions = solutions ++ workers.filterNot(w => time < w._2)

      val canBegin = allSteps.filterNot(step => instructions.map(_._2).contains(step)).sorted.filterNot(e => workers.map(_._1).contains(e)) //.head

      val nbSlots = 5 - workers.length

      val nbToAdd = math.min(nbSlots, canBegin.size)
      val toAdd = canBegin.take(nbToAdd)
      workers = workers ++ toAdd.map { s =>
        (s, time + 60 + s.head.toInt - 'A'.toInt + 1)
      }

      println("canBegin:"+canBegin.mkString(","))
      solutions = solutions ++ toAdd
      //instructions = instructions.filterNot(i => toAdd.contains(i._1))
      //println("instr:"+instructions.mkString(","))
      time += 1
    }


    solutions.mkString
    //BFUXELNGIRHQPSJKVTYOCZDWMA
    //BFUXELNGIRHQPSJKVTYOCZDWM
    //BFUXELNRGHIQPSJKVTYOCZDWM
  }


}
