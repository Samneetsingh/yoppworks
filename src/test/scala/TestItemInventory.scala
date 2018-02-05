import org.scalatest._
import ItemInventory._

class TestItemInventory extends FlatSpec {

  "getItemName" should "provide itemName to corresponding itemID" in {
    assert(getItemName(1) === "apple")
  }

  "getItemName" should "provide itemPrice to corresponding itemID" in {
    assert(getItemPrice(1) === 1.0)
  }

}
