import models.{Promotion, PromotionCombo}

object PromotionSolution extends App {

    val promos = Seq(
        Promotion("P1", Seq("P3")),
        Promotion("P2", Seq("P4,P5")),
        Promotion("P3", Seq("P1")),
        Promotion("P4", Seq("P2")),
        Promotion("P5", Seq("P2")),
    )

    private def promosNotAllowedMapping(promos: Seq[Promotion]): Map[String, Seq[String]] =
        promos.groupBy(_.code).map {
            case (k, v) => (k, v.flatMap(_.notCombinableWith))
        }

    private def promosAllowedMapping(promos: Seq[Promotion], allPromos: Seq[String]): Map[String, Seq[String]] =
        promos.groupBy(_.code).map {
            case (k, v) => (k, allPromos.filter(p => !v.flatMap(_.notCombinableWith).contains(p) && p != k))
        }


    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        val promosDenyMap = promosNotAllowedMapping(allPromotions)
        val allPromos: Seq[String] = promosDenyMap.keySet.toSeq
        val promosAllowMap = promosAllowedMapping(allPromotions, allPromos)
        println(getCombinations(allPromos, promosAllowMap, promosDenyMap))
        //println("promos allowed map: " + promosAllowMap)

        Seq()
    }



    def getCombinations(
        allPromos: Seq[String],
        promosAllowMap: Map[String, Seq[String]],
        promosDenyMap: Map[String, Seq[String]]
    ) = {

        val answerMaybe = allPromos.foldLeft((Seq[String](),Seq[String]()))((accumulator, p) => {
            val allowList = if(!accumulator._1.contains(p) && !accumulator._2.contains(p)) {
                accumulator._1 ++ Seq(p)
            } else {
                accumulator._1
            }
            val denyList = if(accumulator._2.contains(p)) accumulator._2 else accumulator._2 ++ promosDenyMap.getOrElse(p, Seq())

            val result = allPromos.foldLeft((Seq[String](),Seq[String]()))((acc, promo) => {
                println("accumulator: " + accumulator + "\t" + p)
                //println("acc: " + acc + "\t" + promo)
                if(acc._1.length + acc._2.length >= allPromos.length) {
                    acc
                }
                val allowList = if(!acc._1.contains(p) && !acc._2.contains(p)) acc._1 ++ Seq(p) else acc._1
                val denyList = if(acc._2.contains(p)) acc._2 else acc._2 ++ promosDenyMap.getOrElse(p, Seq())
                (allowList, denyList)
            })._1
            //accumulator ++ Seq(result)
        })
        println("answer maybe? " + answerMaybe)

        /*for {
            promo1 <- allPromos
            tmpList = Seq(promo1)
            denyList = promosDenyMap.get(promo1) match {
                case Some(actualList) => actualList
                case _ => Seq()
            }

            _ = println(promo1 + "\t")
        } yield ()*/
    }

    private def foldHelper(allPromos: Seq[String], promosDenyMap: Map[String, Seq[String]]) =
        allPromos.foldLeft((Seq[String](),Seq[String]()))((acc, promo) => {
            println("acc: " + acc + "\t" + promo)
            if(acc._1.length + acc._2.length >= allPromos.length) {
                acc
            }
            val allowList = if(!acc._1.contains(promo) && !acc._2.contains(promo)) acc._1 ++ Seq(promo) else acc._1
            val denyList = if(acc._2.contains(promo)) acc._2 else acc._2 ++ promosDenyMap.getOrElse(promo, Seq())
            (allowList, denyList)
        })._1

    private def isEligibleCombination(combo: Seq[String], promosMap: Map[String, Seq[String]]) = {
       combo.flatMap {
           x => combo.flatMap {
               y => if(x != y && !promosMap.get(x).contains(y)) {
                   //println("x|y: " + x + " " + y)
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
