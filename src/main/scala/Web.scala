import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives.{complete, concat, entity, get, path, pathSingleSlash}

import scala.io.StdIn

object Web {
  val myTesting = new MyTesting
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    var serverNodeCreated = false
    // implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end

    implicit val executionContext = system.dispatcher
    val route= get {
      concat(

        pathSingleSlash {
          //actorSystemDriver.ActorSystemDriver
          complete(HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            "<html><body> <a href=\"http://127.0.0.1:8080/addNode\">1. Add a Server Node</a><br> " +
              "<a href=\"http://127.0.0.1:8080/loadData\">2. Delete Node</a><br> " +
              "<a href=\"http://127.0.0.1:8080/lookupData\">3. Lookup Data on Servers by Id</a><br> " +
              "</body></html>"))
        },
        path("addNode") {
          val result = myTesting.bootstrap()
          complete(HttpResponse(entity = HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            "<html><body> Added a node! <br><a href=\"http://127.0.0.1:8080/\">Go Back</a><br><br> </body></html>")))

        })
    }
    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
