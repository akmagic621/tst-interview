import models.{Promotion, PromotionCombo}

object PromotionSolution {

    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = ???

    def combinablePromotions(
                              promotionCode: String,
                              allPromotions: Seq[Promotion]): Seq[PromotionCombo] = ???

}
