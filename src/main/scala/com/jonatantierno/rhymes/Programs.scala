package com.jonatantierno.rhymes

object Declarative{

  trait IO[P[_]]{
    def read(): P[String]
    def write(msg: String): P[Unit]
  }

  object IO{
    def apply[P[_]](implicit IO: IO[P]) = IO

    object Syntax{
      def read[P[_]]()(implicit IO: IO[P]) = IO.read()
      def write[P[_]](msg: String)(implicit IO: IO[P]) = IO.write(msg)
    }
  }

  object Programs extends Sentence with Stress with Syllable with Rhymes { 
    import scalaz.Monad
    import scalaz.syntax.monad._, IO.Syntax._

    def writeMsg[P[_]: IO: Monad](msg: String): P[Unit] = for {
      _ <- write(msg)
    } yield()

    def findRhymes[P[_]: IO: Monad](target: String): P[Unit] = {
      for{
        _ <- write(describeWord(target)) 
        elQuijote <- read
        _ <- write(getRhymes(target, elQuijote))
      } yield()
    }

    def describeWord(word: String): String = {
      val syllables = splitInSyllables(word)
      val prettySyllables = syllables.tail.foldLeft(syllables.head)(_ + "-" + _)
      val stressName = name(syllables)
      s"$prettySyllables ($stressName)\n"
    }

    def getRhymes(target: String, text: String): String = {
      val rhymes = splitInSentences(text).filter(rhymesNoRepeat(_, target)).map(_.replace("\n"," ").replace("\r"," ").trim())

      if (rhymes.length == 0) "No se ha encontrado\n"
      else rhymes.foldLeft("")(_ + "\n" + _).concat("\n")
    }
  }
  
  object APIInstantiation{
    import scalaz.{Monad, Id}, Id.Id
    val quijoteUrl ="http://www.gutenberg.org/cache/epub/2000/pg2000.txt"
    val quijoteFile ="/tmp/elquijote.txt"

    implicit object QuijoteIO extends IO[Id] with Connection{
      import scala.io.StdIn.readLine

      def read() = get(quijoteUrl)
      def write(msg: String) = print(msg)
    }

    implicit object CachedQuijoteIO extends IO[Id] with Connection{
      import scala.io.StdIn.readLine
      import scala.io.Source
      import java.io._

      def read() = cache(quijoteUrl, quijoteFile)
      def write(msg: String) = print(msg)

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

    implicit object IdMonad extends Monad[Id]{
      def point[A](a: => A): Id[A] = a
      def bind[A,B](p: Id[A])(f: A => Id[B]): Id[B] = f(p)
    }
  }
}
