package controllers

import play.api._
import play.api.mvc._
import play.api.cache.Cache
import play.api.Play.current
import com.jonatantierno.rhymes.Verse
import com.jonatantierno.rhymes.Quijote

import play.api.db._

object Application extends Controller with Verse {

  def rhymes = Action {

    Ok(describeWord("bolígrafo"))
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

  def inParagraphs(res: List[String]): String =
    if (res.length == 0) "No se ha encontrado\n"
    else res.foldLeft("")(_ + "\n<p>" +_+ "</p>")

  def rhyme(sentence: List[String]): String = sentence match {
    case List() => "Dame una palabra o frase, y busco frases que rimen en el Quijote." 
    case word :: List() => inParagraphs(getRhymes(word, Quijote.get()))
    case _ => inParagraphs(getVersesAsList(sentence.foldLeft("")(_ + " " + _ ), Quijote.get()))
  }
}
