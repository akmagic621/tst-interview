import models.{Promotion, PromotionCombo}

import scala.annotation.tailrec

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
        val allPromos: Seq[String] = promosMap.keySet.toSeq
        val allCombinations = getAllPossibleCombinations(allPromos).filter(_.length > 1)
        println(allCombinations)

        val allCombinablePromotions = allCombinations.map {
            combo => isEligibleCombination(combo, promosMap)
        }
        println("all combinable: " + allCombinablePromotions)

        //val asdf = recursive(allPromos)
        //println(asdf)
        allCombinablePromotions.filter(_.length > 1).map {
            cp => PromotionCombo(cp)
        }
    }

    private def isEligibleCombination(combo: Seq[String], promosMap: Map[String, Seq[String]]) = {
       combo.flatMap {
           x => combo.flatMap {
               y => if(!promosMap.get(x).contains(y)) {
                   println("x|y: " + x + " " + y)
                   combo
               } else {
                   Seq()
               }
           }
       }
    }

    private def getAllPossibleCombinations(allPromos: Seq[String]): Seq[Seq[String]] = {
        allPromos.toSet[String].subsets.map(_.toSeq).toSeq
    }


    private def recursive(values: List[String], acc: List[String] = List.empty): Seq[String] = {
        values match {
            case Nil => acc
            case value :: t => recursive(t, value :: acc)
        }
    }
    println(allCombinablePromotions(promos))

    def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = ???
}
