
import models.TestScenarios.getAllTests
import models.{BestGroupPrice, CabinPrice, Rate}

object BestGroupPriceSolution extends App {

    private def getRateCodeToRateGroupMap(rates: Seq[Rate]): Map[String, Seq[String]] =
        rates.groupBy(_.rateCode).map {
            case (k, v) => (k, v.map(_.rateGroup))
        }

    def getBestGroupPrices(cabinPriceList: Seq[CabinPrice], rates: Seq[Rate]) = {
        val rateCodeToGroupMap = getRateCodeToRateGroupMap(rates)
        val allPricesList = cabinPriceList.map {
            cp => BestGroupPrice(cp.cabinCode, cp.rateCode, cp.price, rateCodeToGroupMap(cp.rateCode).head)
        }

        val groupedAllPricesList = allPricesList.groupBy(
            ap => (ap.cabinCode, ap.rateGroup)
        )
        val groupedAndSortedPricesList = groupedAllPricesList.map {
            x => x._2.sortBy(_.price)
        }
        /* the sorting at the end of this below is unnecessary if we don't care about the order,
            I just wanted to ensure the exact same output that was provided in the assignment
         */
        groupedAndSortedPricesList.map(_.head).toSeq.sortBy(bg => (bg.cabinCode, bg.rateCode))
    }

    val allTests = getAllTests

    allTests.map(
        test => assertEq[BestGroupPrice](getBestGroupPrices(test.model2, test.model1), test.expectedOutput)
    )

    def assertEq[M](actual: Seq[M], expected: Seq[M]): Unit = {
        if (actual.equals(expected)) {
            println("Output matches for this test case! Great success!")
        } else if (actual.size != expected.size) {
            println(
                s"ERROR SIZE_NOT_EQ expectedSize=${expected.size} != actualSize=${actual.size}\n"+
                  s"expected=$expected is not actual=$actual"
            )
        } else {
            println(s"ERROR expected=$expected did not equal actual=$actual")
        }
    }

}


