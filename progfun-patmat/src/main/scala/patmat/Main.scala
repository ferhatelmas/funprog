package patmat

object Main {
	def main(args: Array[String]): Unit = {
	  println(Huffman.encode(Huffman.frenchCode)(Huffman.decodedSecret))
	  
	  val l = "hello world asd fasd asa".toList
	  val t = Huffman.createCodeTree(l)
	  println(Huffman.encode(t)(l).length)
	  println(Huffman.decode(t, Huffman.encode(t)(l)))
	  println(Huffman.times(l))
	  println(Huffman.makeOrderedLeafList(Huffman.times(l)))
	  println(Huffman.until(Huffman.singleton, Huffman.combine)(Huffman.makeOrderedLeafList(Huffman.times(l))))
	  println(Huffman.decodedSecret)
	}
}