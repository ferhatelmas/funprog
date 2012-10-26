package recfun
import common._

object Main {
  def main(args: Array[String]) {
    // unit tests
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println()
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    
    println()
    
    println(countChange(4, List(1, 2)))
    println(countChange(0, List(1, 2)))
    println(countChange(4, List()))
  }

  def pascal(c: Int, r: Int): Int = 
    if (c == r || c == 0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  def balance(chars: List[Char]): Boolean = {
    def count(chars: List[Char], cnt: Int): Boolean = {
    	if (chars.isEmpty) cnt == 0
    	else if (chars.head == '(') count(chars.tail, cnt+1)
    	else if (chars.head == ')') {
    	  if (cnt == 0) false
    	  else count(chars.tail, cnt-1) 
    	}
    	else count(chars.tail, cnt)
    }
    count(chars, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int = 
    if (money == 0) 1
    else if (coins.isEmpty) 0 
    else if (money >= coins.head) countChange(money-coins.head, coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)
}
