package org.kapunga.icelandic

import org.kapunga.icelandic.morph._

/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
sealed trait Lemma
case class Headword(lemma: String, gram: Seq[String], orth: Option[String]) extends Lemma
case class MaleNoun(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class FemaleNoun(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class NeuterNoun(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class Verb(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class Adjective(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma

object Lemma {
  def apply(headWord: String, gram: Seq[String], orth: Option[String]): Lemma =
    gram.toSet match {
      case g if g.contains("m")   => MaleNoun(headWord, g - "m", orth)
      case g if g.contains("f")   => FemaleNoun(headWord, g - "f", orth)
      case g if g.contains("n")   => NeuterNoun(headWord, g - "n", orth)
      case g if g.contains("v")   => Verb(headWord, g - "v", orth)
      case g if g.contains("adj") => Adjective(annotateAdj(headWord), g - "adj", orth)
      case _                      => Headword(headWord, gram, orth)
    }
}
