package models

case class Promotion(
    code: String,
    notCombinableWith: Seq[String]
)
