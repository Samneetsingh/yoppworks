import scala.util.control.Breaks._

object DiscountProcessing {
  def check(d: Int, itemBag: scala.collection.mutable.Map[Int, Int]): Boolean = {
    var check = false
    val dParam = DiscountInventory.getConditionParam(d)
    breakable(
      for ( item <- dParam ) {
        if ( itemBag.exists(_._1 == item._1) ){
          if (itemBag(item._1) < item._2) {
            check = false
            break
          }
          else {
            check = true
          }
        }
        else {
          check = false
        }
      }
    )
    check
  }

  def calculateDiscountValue(d: Int, itemBag: scala.collection.mutable.Map[Int, Int]): Double = {
    val dType = DiscountInventory.getDiscountType(d)
    val dOn = DiscountInventory.getDiscountOnItem(d)
    val dValue = DiscountInventory.getDiscountValue(d)
    val amount = dType match {
      case "%" => dOn.map( item => ItemInventory.getItemPrice(item._1) * item._2 * dValue ).foldLeft(0.0)(_ + _)
      case "+" => dOn.map( item => ItemInventory.getItemPrice(item._1) * itemBag(item._1) * dValue ).foldLeft(0.0)(_ + _)
      case _ => 0.0
    }
    amount
  }

  def getMaxDiscountCombination(discount: List[Int], itemBag: Map[Int, Int]): Double = {
    var maxDiscount = 0.0
    for ( dPerm <- discount.permutations) {
      var tempOrder = scala.collection.mutable.Map() ++= itemBag
      var discount = 0.0
      for (d <- dPerm) {
        val dCondition = DiscountInventory.getConditionParam(d)
        if (check(d, tempOrder)) {
          discount += calculateDiscountValue(d, tempOrder)
        }
        for ( item <- dCondition) {
          tempOrder.update(item._1, tempOrder(item._1) - item._2)
        }
      }
      if (discount > maxDiscount) {
        maxDiscount = discount
      }
    }
    maxDiscount
  }

  def getApplicableDiscounts(itemBag: Map[Int, Int]): Double = {
    val tempOrder = scala.collection.mutable.Map() ++= itemBag
    val discounts = DiscountInventory.getDiscountList.filter( d => check(d, tempOrder))
    getMaxDiscountCombination(discounts, itemBag)
  }

  def calculateLowestPrice(itemBag: Map[Int, Int]): Double = {
    itemBag.map( item => ItemInventory.getItemPrice(item._1) * item._2).foldLeft(0.0)(_ + _) - getApplicableDiscounts(itemBag)
  }

  def main(args: Array[String]): Unit = {
    val order = List((1,3), (2,1), (3,2), (4,1)).toMap
    println(calculateLowestPrice(order))
  }

}
