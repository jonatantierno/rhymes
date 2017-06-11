package com.jonatantierno.rhymes

object Main {

  def main(args: Array[String]): Unit = {
    import Declarative._
    import APIInstantiation.{CachedQuijoteIO, IdMonad}
    import Programs.{writeMsg, findRhymes, findVerses}
    import WebServer._

    if (args.length == 0 ) start() 
    else if (args.length == 1 && args(0) == "help") writeMsg(
        "Escribe una palabra, y la separo en sílabas y busco frases que rimen en el quijote.\n" + 
        "Escribe un verso, y busco otros."
        );
    else if (args.length == 1) findRhymes(args(0))
    else findVerses(args)
  }
}
