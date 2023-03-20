object Day21 extends MultiPuzzle[Int, Int] {
  override def part1(input: Iterator[String]): Int = {
    val foods: List[(Set[String], Set[String])] = input.map { line =>
      val s"$ingredients_ (contains $allergens_)" = line
      (ingredients_.split(' ').toSet, allergens_.split(", ").toSet)
    }.toList

    val allergens = foods.map(_._2).reduce(_.union(_))

    val allergen2possible: Map[String, Set[String]] = allergens.toList.map { allergen =>
      (allergen, foods.filter(_._2.contains(allergen)).map(_._1).reduce(_.intersect(_)))
    }.toMap

    var m = allergen2possible

    while (m.exists(_._2.size >= 1)) {
      val found: Option[(String, Set[String])] = m.find(_._2.size == 1)
      found match {
        case Some((allergen, ingredients)) =>
          val ingredient = ingredients.head
          println(s"$allergen->$ingredient")
          m = m.view.mapValues(_ - ingredient).toMap
        case None => sys.error("illegal state")
      }

    }

    println(allergens)

    val ingredientsWithAllergen =
      Set("jrhvk", "lfcppl", "jhsrjlj", "qjltjd", "xslr", "lkv", "rfpbpn", "zkls")

    foods.map { case (ingr, _) => ingr.filter(i => !ingredientsWithAllergen.contains(i)).size }.sum

    // TODO : build the list of allergens
    // TODO : for each allergen, list possible ingredients

  }

  override def part2(input: Iterator[String]): Int = 43
}

//lkv,lfcppl,jhsrjlj,jrhvk,zkls,qjltjd,xslr,rfpbpn
