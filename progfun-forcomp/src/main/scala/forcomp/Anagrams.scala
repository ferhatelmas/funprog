package forcomp

import common._

object Anagrams {

  def main(args: Array[String]): Unit = {
    println(sentenceAnagrams(List("yes", "man")) mkString "\n")
  }
  
  type Word = String
  
  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = 
	w.groupBy(c => c.toLower).toList.map(e => (e._1, e._2.length)).sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s mkString)
    
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = 
    dictionary groupBy(w => wordOccurrences(w))

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
  
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case x :: xs => {
      val xc: Occurrences = (for { o <- 1 to x._2 } yield (x._1, o)).toList
      val xsc = combinations(xs)
      (xsc ::: (for{l <- xc} yield List(l)) ::: (for { l1 <- xsc; l2 <- xc } yield l2 :: l1)) distinct
    }
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = y.foldLeft(
    x.foldLeft(Map[Char, Int]())((m, e) => m + e))(
      (m1, e1) => if(m1(e1._1) - e1._2 > 0) m1.updated(e1._1, m1(e1._1)-e1._2) 
       			  else m1 - e1._1).toList.sorted
    
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = { 
    def anagrams(l: Occurrences): List[Sentence] = l match {
      case List() => List(List())
      case _ => {
        for {
	      o <- combinations(l)
	      if(dictionaryByOccurrences.contains(o))
	        s <- anagrams(subtract(l, o))
	        w <- dictionaryByOccurrences(o)
	    } yield w :: s
      }
    }
    
    anagrams(sentenceOccurrences(sentence))
  }
}
