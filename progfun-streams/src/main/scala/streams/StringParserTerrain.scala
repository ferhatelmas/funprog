package streams

import common._

trait StringParserTerrain extends GameDef {

  val level: String

  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {
    p: Pos => 
      if(p.x < 0 || p.x >= levelVector.size) false
      else {
        val vertical = levelVector(p.x)
        if(p.y < 0 || p.y >= vertical.size) false
        else vertical(p.y) != '-'
      }
  }

  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    def findCharX(x: Int, vec: Vector[Vector[Char]]): Pos = {
      val y = vec.head.indexOf(c)
      if(y == -1) findCharX(x+1, vec.tail)
      else Pos(x, y)
    }
    
    findCharX(0, levelVector)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar('S', vector)
  lazy val goal: Pos = findChar('T', vector)

}
