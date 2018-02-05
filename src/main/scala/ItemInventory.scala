object ItemInventory {

  val ItemsLookUp: Map[Int, (String, Double)] = Map(
    1 -> ("apple", 1.0),
    2 -> ("bread", 1.0),
    3 -> ("butter", 1.0),
    4 -> ("cheese", 1.0)
  )

  def getItemName(itemID: Int): String = {
    ItemsLookUp(itemID)._1
  }

  def getItemPrice(itemID: Int): Double = {
    ItemsLookUp(itemID)._2
  }


}
