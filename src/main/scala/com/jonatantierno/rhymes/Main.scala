package com.jonatantierno.rhymes

object Main extends Syllable with Connection with Sentence{
  def main(args: Array[String]): Unit = {
    if (args.length == 0 ) println("Escribe una palabra, la separo en sílabas y la busco en el quijote.");
    else {
      val target = args(0)
      val syllables = splitInSyllables(target)
      println(syllables.tail.foldLeft(syllables.head)(_ + "-" + _))
      val elQuijote = get("http://www.gutenberg.org/cache/epub/2000/pg2000.txt")
        val rhymes = splitInSentences(elQuijote).filter(_.endsWith(target)).map(_.trim).map(_.replace("\n"," ").replace("\r",""))

      if (rhymes.length == 0) println("No se ha encontrado")
      else rhymes.foreach(println(_))
    }
  }
}
