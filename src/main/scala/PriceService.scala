object PriceService {

  /*
  *
  */
  def calculateLowestPrice(items: List[(Int, Int)]): Double = {
    val order = items.toMap
    val dp = new DiscountProcessing()
    order.map( item => ItemInventory.getItemPrice(item._1) * item._2).foldLeft(0.0)(_ + _) - dp.getApplicableDiscounts(order)
  }

  def main(args: Array[String]): Unit = {
    val items = List((1,3), (2,1), (3,4), (4,1))
    println(calculateLowestPrice(items))
  }

}
