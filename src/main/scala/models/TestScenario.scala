package models

case class TestScenario[A,B,C](
    model1: Seq[A],
    model2: Seq[B],
    expectedOutput: Seq[C]
)

object TestScenarios {
    val testCase1: TestScenario[Rate, CabinPrice, BestGroupPrice] =
        TestScenario(
            model1 = Seq(
                Rate("M1","Military"),
                Rate("M2","Military"),
                Rate("S1","Senior"),
                Rate("S2","Senior")
            ),
            model2 = Seq(
                CabinPrice("CA","M1",200.00),
                CabinPrice("CA","M2",250.00),
                CabinPrice("CA","S1",225.00),
                CabinPrice("CA","S2",260.00),
                CabinPrice("CB","M1",230.00),
                CabinPrice("CB","M2",260.00),
                CabinPrice("CB","S1",245.00),
                CabinPrice("CB","S2",270.00)
            ),
            expectedOutput = Seq(
                BestGroupPrice("CA", "M1", 200.00, "Military"),
                BestGroupPrice("CA", "S1", 225.00, "Senior"),
                BestGroupPrice("CB", "M1", 230.00, "Military"),
                BestGroupPrice("CB", "S1", 245.00, "Senior")
            )
        )

    def getAllTests: Seq[TestScenario[Rate, CabinPrice, BestGroupPrice]] = Seq(testCase1)
}