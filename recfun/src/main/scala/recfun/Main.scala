package recfun

object Main {
//  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
//  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balanced(chars: List[Char], isOpen: Int): Boolean = {
        if (chars.isEmpty) isOpen == 0
        else
          if (chars.head == '(') balanced(chars.tail, isOpen + 1)
        else
          if (chars.head == ')') isOpen > 0 && balanced(chars.tail, isOpen - 1)
        else balanced(chars.tail, isOpen)
      }
      balanced(chars,0)

    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def f(lastMaxCoin_total_coll: List[(Int, Int)], count: Int): Int = {
      if (lastMaxCoin_total_coll.isEmpty) {
        count
      } else {
        val b = scala.collection.mutable.ListBuffer[(Int, Int)]()
        var newCount = count
        for ((lastMaxCoin, total) <- lastMaxCoin_total_coll) {
          if (total < money) {
            for (coin <- coins) {
              if (coin >= lastMaxCoin) {
                val e = (coin, total + coin)
                b += e
              }
            }
          } else if (total == money) {
            newCount += 1
          }
        }

        f(b.toList, newCount)
      }
    }

    val b = coins.map { c => (c, c) }
    f(b, 0)
  }
  }
