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

  object Programs extends Verse with Stress with Syllable with Rhymes { 
    import scalaz.Monad
    import scalaz.syntax.monad._, IO.Syntax._

    def writeMsg[P[_]: IO: Monad](msg: String): P[Unit] = for {
      _ <- write(msg)
    } yield()

    def findRhymes[P[_]: IO: Monad](target: String): P[Unit] = {
      for{
        _ <- write(describeWord(target)) 
        elQuijote <- read
        _ <- write(getRhymesAsString(target, elQuijote))
      } yield()
    }

    def findVerses[P[_]: IO: Monad](verse: Array[String]): P[Unit] = {
      val target: String = verse.reduce(_ + " " + _)
      for{
        _ <- write(describeWord(lastWord(target).replace(" ","")) )
        elQuijote <- read
        _ <- write(getVerses(target, elQuijote))
      } yield()
    }
}
  
  object APIInstantiation {
    import scalaz.{Monad, Id}, Id.Id

    implicit object QuijoteIO extends IO[Id] {
      import scala.io.StdIn.readLine

      def read() = Quijote.get()
      def write(msg: String) = print(msg)
    }

    implicit object CachedQuijoteIO extends IO[Id] {
      def read() = Quijote.getCached()
      def write(msg: String) = print(msg)
    }

    implicit object IdMonad extends Monad[Id]{
      def point[A](a: => A): Id[A] = a
      def bind[A,B](p: Id[A])(f: A => Id[B]): Id[B] = f(p)
    }
  }
}
