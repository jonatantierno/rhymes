package controllers

import play.api._
import play.api.mvc._
import play.api.cache.Cache
import play.api.Play.current
import com.jonatantierno.rhymes.Verse
import scala.io.Source

import play.api.db._

object Application extends Controller with Verse {

  val quijote = Source.fromFile(Play.getFile("conf/quijote.txt")).getLines.mkString

  def rhymes(sentence: String) = Action {
    System.out.println(quijote.substring(0,200))
    Ok(rhyme(sentence)).as(HTML)
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

  def rhyme(sentenceAsString: String): String = splitInSentences(sentenceAsString) match {
    case List() => "Dame una palabra o frase, y busco frases que rimen en el Quijote." 
    case word :: List() => inParagraphs(getRhymes(word, quijote))
    case _ => inParagraphs(getVersesAsList(sentenceAsString.foldLeft("")(_ + " " + _ ), quijote))
  }

  def inParagraphs(res: List[String]): String =
    if (res.length == 0) "No se ha encontrado\n"
    else res.foldLeft("")(_ + "\n<p>" +_+ "</p>")

}
