import org.slf4j.LoggerFactory

object PriceService {

  val logger = LoggerFactory.getLogger(classOf[DiscountProcessing])
  /*
  *
  */
  def calculateLowestPrice(items: List[(Int, Int)], multipleFlag: Boolean = true): Double = {
    val order = items.toMap
    val dp = new DiscountProcessing()
    order.map( item => ItemInventory.getItemPrice(item._1) * item._2).foldLeft(0.0)(_ + _) - dp.getApplicableDiscounts(order)
  }

  def main(args: Array[String]): Unit = {
    val items = List((1,3), (2,1), (3,4), (4,1))
    logger.info(s"Lowest Price: ${calculateLowestPrice(items)}")
  }

}
