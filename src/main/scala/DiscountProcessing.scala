import scala.util.control.Breaks._

class DiscountProcessing {
  def check(discountId: Int, order: scala.collection.mutable.Map[Int, Int]): Boolean = {
    var check = false
    val dParam = DiscountInventory.getConditionParam(discountId)
    breakable(
      dParam.foreach( item => if ( order.exists(_._1 == item._1) ){
        if (order(item._1) < item._2) {
          check = false
          break
        }
        else { check = true }
      }
      else { check = false })
    )
    check
  }

  def calculateDiscountValue(discountId: Int, order: scala.collection.mutable.Map[Int, Int]): Double = {
    val dType = DiscountInventory.getDiscountType(discountId)
    val dOn = DiscountInventory.getDiscountOnItem(discountId)
    val dValue = DiscountInventory.getDiscountValue(discountId)
    val amount = dType match {
      case "%" => dOn.map( item => ItemInventory.getItemPrice(item._1) * item._2 * dValue ).foldLeft(0.0)(_ + _)
      case "+" => dOn.map( item => ItemInventory.getItemPrice(item._1) * order(item._1) * dValue ).foldLeft(0.0)(_ + _)
      case _ => 0.0
    }
    amount
  }

  def getMaxDiscountCombination(discounts: List[Int], order: Map[Int, Int]): Double = {
    var maxDiscountAmount = 0.0
    for ( dPerm <- discounts.permutations) {
      var tempOrder = scala.collection.mutable.Map() ++= order
      var discountAmount = 0.0
      for (d <- dPerm) {
        val dCondition = DiscountInventory.getConditionParam(d)
        if (check(d, tempOrder)) {
          discountAmount += calculateDiscountValue(d, tempOrder)
        }
        for ( item <- dCondition) {
          tempOrder.update(item._1, tempOrder(item._1) - item._2)
        }
      }
      if (discountAmount > maxDiscountAmount) {
        maxDiscountAmount = discountAmount
      }
    }
    maxDiscountAmount
  }

  def getNoOfDiscountPerItem(discountId: Int, order: Map[Int, Int]): List[Int] = {
    DiscountInventory.getDiscountType(discountId) match {
      case "%" =>
        val tempOrder = scala.collection.mutable.Map() ++= order
        var discounts: List[Int] = List()
        do {
          val dCondition = DiscountInventory.getConditionParam(discountId)
          dCondition.foreach( item => tempOrder.update(item._1, tempOrder(item._1) - item._2))
          discounts ::= discountId
        } while (check(discountId, tempOrder))
        discounts
      case _ => List(discountId)
    }
  }

  def getApplicableDiscounts(order: Map[Int, Int]): Double = {
    val tempOrder = scala.collection.mutable.Map() ++= order
    val discounts = DiscountInventory.getDiscountList.filter( d => check(d, tempOrder))
    val applicableDiscounts = discounts.map( d => getNoOfDiscountPerItem(d, order)).foldRight(List[Int]())(_ ::: _)
    println(applicableDiscounts)
    getMaxDiscountCombination(applicableDiscounts, order)
  }

}
