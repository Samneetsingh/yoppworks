object DiscountInventory extends DiscountInventoryTrait {
  // id -> condition, conditionValue, discountOn, discountValue
  override val DiscountLookUp: Map[Int, (String, List[(Int, Int)], List[(Int, Int)], Double)] = Map(
    1 -> ("+", List((1, 2)), List((1,1)), 0.5),
    2 -> ("%", List((2,1), (3,2)), List((3,2)), 0.5),
    3 -> ("%", List((3,1), (4,1)), List((3,1), (4,1)), 1.0)
  )

  override def getDiscountType(id: Int): String = {
    DiscountLookUp(id)._1
  }

  override def getConditionParam(id: Int): List[(Int, Int)] = {
    DiscountLookUp(id)._2
  }

  override def getDiscountOnItem(id: Int): List[(Int, Int)] = {
    DiscountLookUp(id)._3
  }

  override def getDiscountValue(id: Int): Double = {
    DiscountLookUp(id)._4
  }

  override def getDiscountList: List[Int] = {
    DiscountLookUp.keys.toList
  }

}
