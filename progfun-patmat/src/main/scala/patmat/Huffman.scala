package patmat

import common._
import collection.breakOut

object Huffman {

  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w 
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, c, _) => c
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = 
    chars.map(c => (c, chars.count(x => x==c))) distinct

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = 
    freqs.sortWith((p1, p2) => (p1._2 < p2._2)).map(p => Leaf(p._1, p._2))

  def singleton(trees: List[CodeTree]): Boolean = trees.length == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = 
    if(trees.length < 2) trees
    else (makeCodeTree(trees.head, trees.tail.head) :: trees.tail.tail)
    	.sortWith((t1, t2) => weight(t1) < weight(t2))
  
  def until(xxx: List[CodeTree] => Boolean, yyy: List[CodeTree] => List[CodeTree])(zzz: List[CodeTree]): List[CodeTree] =
    if(xxx(zzz)) zzz
    else until(xxx, yyy)(yyy(zzz))
  
  def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = decodeIn(tree, tree, bits)
  
  def decodeIn(tree: CodeTree, abs: CodeTree, bits: List[Bit]): List[Char] = tree match {
    case Leaf(c, _) => c :: decode(abs, bits)
    case Fork(l, r, _, _) => if(bits.isEmpty) List() else decodeIn((if(bits.head == 0) l else r), abs, bits.tail)
  }
      
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = 
    if(text.isEmpty) List()
    else encodeIn(tree)(text.head) ::: encode(tree)(text.tail)
  
  def encodeIn(tree: CodeTree)(text: Char): List[Bit] = tree match {
    case Leaf(c, _) => List()
    case Fork(l, r, _, _) => if(chars(l).contains(text)) 0 :: encodeIn(l)(text) else 1 :: encodeIn(r)(text)
  }

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(p => p._1 == char).head._2

  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(c, _) => List((c, List()))
    case Fork(l, r, _, _) => mergeCodeTables(convert(l).map(p => (p._1, 0 :: p._2)), convert(r).map(p => (p._1, 1 :: p._2)))
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = (a ::: b).groupBy{_._1}.map{_._2.head}(breakOut)

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val t = convert(tree)
    text.flatMap(c => codeBits(t)(c))
  }
}
