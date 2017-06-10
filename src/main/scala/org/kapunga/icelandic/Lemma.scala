package org.kapunga.icelandic

import org.kapunga.icelandic.morph._

/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
sealed trait Lemma
case class Headword(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class MaleNoun(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class FemaleNoun(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class NeuterNoun(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class Verb(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma
case class Adjective(lemma: String, gram: Set[String], orth: Option[String]) extends Lemma

object Lemma {
  def apply(headWord: String, gram: Set[String], orth: Option[String]): Lemma =
    gram match {
      case g if g.contains("m")   => MaleNoun(headWord, gram - "m", orth)
      case g if g.contains("f")   => FemaleNoun(headWord, gram - "f", orth)
      case g if g.contains("n")   => NeuterNoun(headWord, gram - "n", orth)
      case g if g.contains("v")   => Verb(headWord, gram - "v", orth)
      case g if g.contains("adj") => Adjective(annotateAdj(headWord), gram - "adj", orth)
      case _                      => Headword(headWord, gram, orth)
    }
}
