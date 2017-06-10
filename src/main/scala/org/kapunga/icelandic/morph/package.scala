package org.kapunga.icelandic

/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
package object morph {
  val vowelSet: Set[Char] = "aáeéiíoóuúyýæö" match { case x => (x + x.toUpperCase).toSet }
  val nonAVowelSet: Set[Char] = (vowelSet - 'a') - 'A'
  val aShiftMap: Map[Char, Char] = Map('a' -> 'ö', 'A' -> 'Ö')

  val adjSuffix: List[String] = "ur" :: "l" :: "n" :: "r" :: Nil

  def annotateAdj(headword: String): String =
    adjSuffix.find(!headword.contains("/") && headword.endsWith(_))
    .map(s => headword.dropRight(s.length) + "/" + s)
    .getOrElse(headword)
}
