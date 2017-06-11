package com.jonatantierno.rhymes

object Quijote extends Connection {
    import scala.io.Source
    import java.io._

    val quijoteUrl ="http://www.gutenberg.org/cache/epub/2000/pg2000.txt"
    val quijoteFile ="/tmp/elquijote.txt"

    def get() : String = get(quijoteUrl)
    def getCached() : String = cache(quijoteUrl, quijoteFile)

      private def cache(url: String, file: String): String = {
        val cacheFile = new File(file)
        if (cacheFile.exists) Source.fromFile(cacheFile).getLines.mkString(" ")
        else {
          val text = get(url) 
          save(text, cacheFile)
          text
        }
      }

      private def save(text:String,file:File): Unit = {
          val bw = new BufferedWriter(new FileWriter(file))
          bw.write(text)
          bw.close()
      }
}
