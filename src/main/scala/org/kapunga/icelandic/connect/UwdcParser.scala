package org.kapunga.icelandic.connect

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Document, Element}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import org.kapunga.icelandic.Lemma
import org.kapunga.icelandic.connect.UwdcConnector.{fetchFromFrag, fetchWord}

import scala.util.Try

/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
object UwdcParser {
  def rawDoc(word: String): Document = JsoupBrowser().parseString(fetchWord(word))

  def definition(word: String): List[Element] = definition(word, rawDoc(word))

  def definition(word: String, doc: Document): List[Element] =
    (doc >?> element(".entry"))
      .flatMap(catchReference)
      .map(List(_)).getOrElse(filterDefList(word, findDefList(word, doc)))

  def catchReference(elem: Element): Option[Element] =
    if ((elem >> text).contains("->") && (elem >> elementList(".sense")).isEmpty)
      JsoupBrowser().parseString(fetchFromFrag(elem >> attr("href")("a"))) >?> element(".entry")
    else
      Some(elem)

  def findDefList(word: String, doc: Document): List[Element] =
    (doc >?> element(".results")).map(_ >> elementList(".lemma")).getOrElse(List())

  def filterDefList(word: String, list: List[Element]): List[Element] =
    list.map(extractLemmaUrl).filter(_._1 == word)
      .flatMap(u => definition(word, JsoupBrowser().parseString(fetchFromFrag(u._2))))

  def extractLemmaUrl(elem: Element): (String, String) =
    (clean((elem >?> element("sup") >> text).getOrElse(""), elem >> text), elem >> attr("href")("a"))

  def clean(sup: String, lemma: String): String = lemma.substring(sup.length).replaceAll("/", "").replaceAll("·", "")

  def headword(elem: Element): Element = elem >> element(".headwd")

  def lemma(elem: Element): String = cleanLemma(Try(headword(elem) >> element(".lemma") >> element("sup") >> text)
    .getOrElse(""), headword(elem) >> element(".lemma") >> text)

  def cleanLemma(sup: String, lemma: String): String = lemma.substring(sup.length)

  def grammar(elem: Element): Seq[String] = (headword(elem) >> elementList(".gram")).map(_ >> text)

  def orth(elem: Element): Option[String] =
    (headword(elem) >> elementList(".orth")).map(_ >> text) match {
      case Nil => None
      case a   => Some(a.reduce(_ + "; " + _))
    }

  def parseDefinition(elem: Element): Option[Lemma] =
    Try(Lemma(lemma(elem), grammar(elem), orth(elem))).toOption

  def findLemma(word: String): List[Lemma] = definition(word).flatMap(parseDefinition)
}
