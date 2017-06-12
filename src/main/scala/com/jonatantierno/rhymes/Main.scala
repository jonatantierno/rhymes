package com.jonatantierno.rhymes

object Main {

  def main(args: Array[String]): Unit = {
    import Declarative._
    import APIInstantiation.{CachedQuijoteIO, IdMonad}
    import Programs.{writeMsg, findRhymes, findVerses}
    import WebServer._

    args match {
        case Array() =>  writeMsg(
            "Escribe una palabra, y la separo en sílabas y busco frases que rimen en el quijote.\n" + 
            "Escribe un verso, y busco otros."
        );
        case Array("start", port) => start(port.toInt)
        case Array("start") => start(8080)
        case Array(oneWord) => findRhymes(oneWord)
        case severalWords => findVerses(severalWords)
    }
  }
}
