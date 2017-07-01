package controllers

import play.api._
import play.api.mvc._
import play.api.cache.Cache
import play.api.Play.current
import com.jonatantierno.rhymes.Verse
import scala.io.Source

import play.api.db._

object Application extends Controller with Verse {

  val quijote = Source.fromFile(Play.getFile("conf/quijote.txt")).getLines.mkString(" ")
  val quijoteList = splitInSentences(quijote)

  def rhyme(sentence: String) = Action {
    Ok(rhymes(sentence)).as(HTML)
  }

  def rhymeStrict(sentence: String) = Action {
    Ok(rhymesStrict(sentence)).as(HTML)
  }
  
  def index = Action {
    Ok(views.html.index(null))
  }

  def db = Action {
    var out = ""
    val conn = DB.getConnection()
    try {
      val stmt = conn.createStatement

      stmt.executeUpdate("CREATE TABLE IF NOT EXISTS ticks (tick timestamp)")
      stmt.executeUpdate("INSERT INTO ticks VALUES (now())")

      val rs = stmt.executeQuery("SELECT tick FROM ticks")

      while (rs.next) {
        out += "Read from DB: " + rs.getTimestamp("tick") + "\n"
      }
    } finally {
      conn.close()
    }
    Ok(out)
  }

  def rhymes(sentence: String): String = inParagraphs(findRhymesInListAsList(sentence, quijoteList))
  def rhymesStrict(sentence: String): String = inParagraphs(findRhymesInListAsListStrict(sentence, quijoteList))

  def inParagraphs(res: List[String]): String =
    if (res.length == 0) "No se ha encontrado\n"
    else res.foldLeft("")(_ + "\n<p>" +_+ "</p>")

}
