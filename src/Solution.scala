import scala.annotation.tailrec

object Solution {

  @tailrec
  def findBest(list: List[(Long, Int)], bestYet: Long): Long = {
    if (list.isEmpty) bestYet
    else {
      val head = list.head
      val buyingPrice = head._1
      val buyingIndex = head._2

      val result = list.tail.find(row => row._2 > buyingIndex)

      result match {
        case Some((sellingPrice, _)) =>
          val loss = buyingPrice - sellingPrice
          val better = if (bestYet > loss) loss else bestYet
          findBest(list.tail, better)
        case None => findBest(list.tail, bestYet)
      }
    }
  }

  def minimumLoss(price: Array[Long]): Int = {
    val priceList = price.toList
    val sorted = priceList.zipWithIndex.sorted.reverse

    findBest(sorted, Long.MaxValue).toInt
  }

  def main(args: Array[String]): Unit = {
    val result = minimumLoss(Array(20, 7, 8, 2, 5))
    val expected = 2

    println(s"Result is: $result")
    println(s"Expected value is: $expected")
    println(s"Is expected equals result? ${result == expected}")
  }
}
