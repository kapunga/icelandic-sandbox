package org.kapunga.icelandic.connect

import org.kapunga.icelandic.connect.UwdcConnector._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Document, Element}
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{attr, element, elementList}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import org.kapunga.icelandic.Lemma

import scala.util.Try

/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
object UWDC {
  def definition(word: String): Option[Element] = {
    def rawDoc(word: String): Document = JsoupBrowser().parseString(fetchWord(word))

    def definition(word: String, doc: Document): Option[Element] =
      (doc >> elementList(".entry")).headOption
        .orElse(findDefList(word, doc).flatMap(filterDefList(word, _)))

    def findDefList(word: String, doc: Document): Option[List[Element]] =
      Try(doc >> element(".results")).toOption.map(_ >> elementList(".lemma"))

    def filterDefList(word: String, list: List[Element]): Option[Element] =
      list.map(extractLemmaUrl)
        .find(_._1 == word)
        .flatMap(u => definition(word, JsoupBrowser().parseString(fetchFromFrag(u._2))))

    def extractLemmaUrl(elem: Element): (String, String) =
      (clean(Try(elem >> element("sup") >> text).getOrElse(""), elem >> text), elem >> attr("href")("a"))

    def clean(sup: String, lemma: String): String = lemma.substring(sup.length).replaceAll("/", "").replaceAll("·", "")

    definition(word, rawDoc(word))
  }

  def parseDefinition(elem: Element): Option[Lemma] = {
    def headword(elem: Element): Element = elem >> element(".headwd")

    def lemma(elem: Element): String = cleanLemma(Try(headword(elem) >> element(".lemma") >> element("sup") >> text)
      .getOrElse(""), headword(elem) >> element(".lemma") >> text)

    def cleanLemma(sup: String, lemma: String): String = lemma.substring(sup.length)

    def grammar(elem: Element): Set[String] = (headword(elem) >> elementList(".gram")).map(_ >> text).toSet

    def orth(elem: Element): Option[String] = Try(headword(elem) >> element(".orth") >> text).toOption

    Try(Lemma(lemma(elem), grammar(elem), orth(elem))).toOption
  }

  def findLemma(word: String): Option[Lemma] = definition(word).flatMap(parseDefinition)
}
