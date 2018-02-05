
trait ItemInventoryTrait {

  val ItemsLookUp: Map[Int, (String, Double)]

  def getItemName(itemID: Int): String

  def getItemPrice(itemID: Int): Double
}
