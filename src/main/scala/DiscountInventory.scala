object DiscountInventory {
  // id -> condition, conditionValue, discountOn, discountValue
  val DiscountLookUp: Map[Int, (String, List[(Int, Int)], List[(Int, Int)], Double)] = Map(
    1 -> ("+", List((1, 2)), List((1,1)), 0.5),
    2 -> ("%", List((2,1), (3,2)), List((3,2)), 0.4),
    // discountOn = 0 means discount on total
    3 -> ("%", List((3,1), (4,1)), List((3,1), (4,1)), 1.0)
  )

  def getDiscountType(id: Int): String = {
    DiscountLookUp(id)._1
  }

  def getConditionParam(id: Int): List[(Int, Int)] = {
    DiscountLookUp(id)._2
  }

  def getDiscountOnItem(id: Int): List[(Int, Int)] = {
    DiscountLookUp(id)._3
  }

  def getDiscountValue(id: Int): Double = {
    DiscountLookUp(id)._4
  }

  def getDiscountList: List[Int] = {
    DiscountLookUp.keys.toList
  }

}
