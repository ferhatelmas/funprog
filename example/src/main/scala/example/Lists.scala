package example

import common._
import java.util.NoSuchElementException

object Lists {
  def sum(xs: List[Int]): Int = 
    if(xs.isEmpty) 0
    else xs.head + sum(xs.tail)

  def max(xs: List[Int]): Int = 
    if(xs.length == 1) xs.head
    else if(xs.isEmpty) throw new NoSuchElementException
    else getBigger(xs.head, max(xs.tail))
  
  def getBigger(a: Int, b:Int): Int = 
    if(a >= b) a
    else b
}
