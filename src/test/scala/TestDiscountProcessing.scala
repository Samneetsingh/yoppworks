import org.scalatest.FlatSpec

class TestDiscountProcessing extends FlatSpec {
  val order: Map[Int, Int] = List((1,3), (2,1), (3,4), (4,1)).toMap

  "getNoOfDiscountPerItem" should "finds frequency of each discount" in {
    val dp = new DiscountProcessing()
    assert(dp.getNoOfDiscountPerItem(1, order) === List(1))
  }

  "calculateDiscountValue" should "calculates the discount amount for a discount" in {
    val dp = new DiscountProcessing()
    val tempOrder = scala.collection.mutable.Map() ++= order
    assert(dp.calculateDiscountValue(1, tempOrder) === 1.5)
  }

  "check" should " checks if discount is applicable or not" in {
    val dp = new DiscountProcessing()
    val tempOrder = scala.collection.mutable.Map() ++= order
    assert(dp.check(1, tempOrder) === true)
  }

  "getMaxDiscountCombination" should " traverse all the combination to find max discount amount" in {
    val dp = new DiscountProcessing()
    val tempOrder = scala.collection.mutable.Map() ++= order
    assert(dp.getMaxDiscountCombination(List(1, 2, 3), order) === 4.5)
  }

}
