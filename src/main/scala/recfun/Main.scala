package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if(c == 0 || c == r) 1
    else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceLoop(chars: List[Char]): Boolean =
    if (chars.isEmpty)  true
    else
    if (chars.head == ')') false
    else
    if (chars.head != '(') balanceLoop(chars.tail)
    else findRight(chars.tail,0)

    def findRight(chars: List[Char],nestLevel : Int): Boolean =
      if (chars.isEmpty) false else
      if (chars.head == '(') findRight(chars.tail,nestLevel+1)
      else if (chars.head == ')') if (nestLevel == 0) balanceLoop(chars.tail) else findRight(chars.tail,nestLevel-1)
      else findRight(chars.tail,nestLevel)

    balanceLoop(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int ={
    def countCoin(coins: List[Int], sum: Int): Int =
    if(coins.isEmpty) 0 else
    if(sum > money) 0
    else if(sum == money) 1
    else countCoin(coins,sum+coins.head)+countCoin(coins.tail,sum)
    countCoin(coins.sortWith(_<_),0)
  }

}
