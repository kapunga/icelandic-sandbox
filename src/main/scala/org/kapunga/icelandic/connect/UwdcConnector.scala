package org.kapunga.icelandic.connect

import java.io.{File, PrintWriter}

import org.kapunga.icelandic.util.md5

import scala.io.Source
import scala.util.Try
import scalaj.http.{Http, HttpRequest}

/**
  * @author Paul J Thordarson kapunga@gmail.com
  */
object UwdcConnector {
  val HEAD_URL: String = "http://digicoll.library.wisc.edu/cgi-bin/IcelOnline/IcelOnline.TEId-idx?"

  def fetchWord(word: String): String =
    fetch(Http(HEAD_URL)
      .param("type", "simple")
      .param("size", "First+100")
      .param("rgn", "lemma")
      .param("q1", word)
      .param("submit", "Search")
      .charset("ISO-8859-1"))

  def fetchFromFrag(urlFrag: String): String =
    fetch(urlFrag.split("\\?")(1)
      .split("&")
      .map(_.split("="))
      .foldLeft(Http(HEAD_URL))((u, p) => u.param(p(0), p(1)))
      .charset("ISO-8859-1"))

  def fetch(request: HttpRequest): String = {
    val urlHash = md5(request.urlBuilder(request))
    loadFromFile(urlHash).getOrElse(Try(cacheResponse(urlHash, request.asString.body)).getOrElse(request.asString.body))
  }

  def loadFromFile(urlHash: String): Option[String] =
    Try(Source.fromFile(cacheFileName(urlHash)).getLines()).map(_.toList.reduce((s1, s2) => s1 + "\n" + s2)).toOption

  def cacheResponse(urlHash: String, responseBody: String): String = {
    val file = new File(cacheFileName(urlHash))
    file.mkdirs()
    file.delete()
    file.createNewFile()

    val pw = new PrintWriter(file)
    pw.write(responseBody)
    pw.close()

    responseBody
  }

  def cacheFileName(urlHash: String): String = cacheDirectory + urlHash + ".txt"
}
