package streams

import common._
import scala.xml.dtd.EMPTY

trait Solver extends GameDef {

  def done(b: Block): Boolean = b.isStanding && b.b1 == goal

  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    val neighbors = b.legalNeighbors
    if(neighbors.isEmpty) Stream.Empty
    else {
      for {
        (block, move) <- neighbors
      } yield (block, move :: history)
    }.toStream
  }

  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = 
                         neighbors.filterNot(p => explored.contains(p._1)) 

  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if(initial.isEmpty) Stream.Empty
    else if(done(initial.head._1)) Stream(initial.head)
    else {
      val newNeighbors = for {
        (block, hist) <- initial
        newNeighbor   <- newNeighborsOnly(neighborsWithHistory(block, hist), explored)
      } yield newNeighbor
      
      initial #::: from(newNeighbors, explored ++ newNeighbors.map(_._1))
    }
  }

  lazy val pathsFromStart: Stream[(Block, List[Move])] = 
    from(Stream((startBlock, List())), Set(startBlock))

  lazy val pathsToGoal: Stream[(Block, List[Move])] = 
    pathsFromStart.filter(p => done(p._1))

  lazy val solution: List[Move] = pathsToGoal match {
    case Stream.Empty => List()
    case s => s.head._2.reverse
  }
}
