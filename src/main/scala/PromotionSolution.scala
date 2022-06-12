import models.{Promotion, PromotionCombo}

object PromotionSolution extends App {

    val promos = Seq(
        Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4,P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")),
        Promotion("P5", Seq("P2")),
    )

    private def getPromotionWithNonCombinableMap(promos: Seq[Promotion]): Map[String, Seq[String]] =
        promos.groupBy(_.code).map {
            case (k, v) => (k, v.flatMap(_.notCombinableWith))
        }

    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        val promosMap = getPromotionWithNonCombinableMap(allPromotions)
        println(promosMap)
        val allPromos: List[String] = promosMap.keySet.toList
        for {
            pr <- allPromos
            notCombinable = promosMap(pr) :+ pr
            _ = println("not combinable: " + notCombinable)
            combinablePromos = allPromos.filter(!notCombinable.contains(_))
            _ = println("combinable for promo " + pr + "\t" + combinablePromos)
        } yield ()
        //val asdf = recursive(allPromos)
        //println(asdf)
        Seq()
    }


    private def recursive(values: List[String], acc: List[String] = List.empty): Seq[String] = {
        values match {
            case Nil => acc
            case value :: t => recursive(t, value :: acc)
        }
    }
    allCombinablePromotions(promos)

    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = ???
}
