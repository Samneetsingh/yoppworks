trait DiscountInventoryTrait {

  val DiscountLookUp: Map[Int, (String, List[(Int, Int)], List[(Int, Int)], Double)]

  def getDiscountType(id: Int): String

  def getConditionParam(id: Int): List[(Int, Int)]

  def getDiscountOnItem(id: Int): List[(Int, Int)]

  def getDiscountValue(id: Int): Double

  def getDiscountList: List[Int]

}
