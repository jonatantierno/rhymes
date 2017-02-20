package com.jonatantierno.rhymes

object Main {

  def main(args: Array[String]): Unit = {
    import Declarative._
    import APIInstantiation.{CachedQuijoteIO, IdMonad}
    import Programs.{writeMsg, findRhymes}

    if (args.length == 0 ) writeMsg("Escribe una palabra, la separo en sílabas y la busco en el quijote.\n");
    else {
      findRhymes(args(0))
    }
  }
}
