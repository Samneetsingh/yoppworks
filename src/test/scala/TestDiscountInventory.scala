
import org.scalatest._
import DiscountInventory._

class TestDiscountInventory extends FlatSpec {

  "getDiscountType" should "provide type corresponding to discountId" in {
    assert(getDiscountType(1) === "+")
  }

  "getConditionParam" should "provide condition parameter corresponding to discountId" in {
    assert(getConditionParam(1) === List((1, 2)))
  }

  "getDiscountValue" should "provide discount percentage corresponding to discountId" in {
    assert(getDiscountValue(1) === 0.5)
  }

  "getDiscountOnItem" should "provide discounted items corresponding to discountId" in {
    assert(getDiscountOnItem(1) === List((1,1)))
  }

  "getDiscountList" should "provide list of available discounts" in {
    assert(getDiscountList === List(1, 2, 3))
  }

}
