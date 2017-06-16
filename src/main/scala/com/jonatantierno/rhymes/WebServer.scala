package com.jonatantierno.rhymes

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.io.StdIn

object WebServer extends Verse {
    def start(port: Int): Unit = {

        implicit val system = ActorSystem("my-system")
        implicit val materializer = ActorMaterializer()
        // needed for the future flatMap/onComplete in the end
        implicit val executionContext = system.dispatcher

        val route =
        pathPrefix("rhyme" / Segments) { sentence =>
            get {
                val result = sentence match {
                    case List() => "Dame una palabra o frase, y busco frases que rimen en el Quijote." 
                    case word :: List() => inParagraphs(getRhymes(word, Quijote.get()))
                    case _ => inParagraphs(getVersesAsList(sentence.foldLeft("")(_ + " " + _ ), Quijote.get()))
                }
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"$result"))
            }
        }

        val bindingFuture = Http().bindAndHandle(route, "localhost", port)

            println(s"Server online at http://localhost:${port}")

            bindingFuture
            .flatMap(_.unbind()) // trigger unbinding from the port
            .onComplete(_ => system.terminate()) // and shutdown when done

            println(s"Server finished!")
    }

    def inParagraphs(res: List[String]): String =
      if (res.length == 0) "No se ha encontrado\n"
      else res.foldLeft("")(_ + "\n<p>" +_+ "</p>")
}

