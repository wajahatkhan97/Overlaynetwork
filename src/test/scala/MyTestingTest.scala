import java.security.MessageDigest

import MyTesting.{MD5, key_actormap, list_of_movies_Titles, list_of_names_to_Assign_to_node, number_nodes, system}
import akka.actor.{ActorSystem, Props}
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.testkit.{TestProbe, filterEvents}
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.mutable

class MyTestingTest extends ScalaTestWithActorTestKit with AnyWordSpecLike{
//create the user actor and check if its returning the correct value



  "Check to see if keys are generating properly" in {
    var size = 6;
    def MD5(s: String) = {
      MessageDigest.getInstance("MD5").digest(s.getBytes())
    }
    val title = "movie1"
    var hashValue_movietitle = MD5(title)

    assert(hashValue_movietitle(0)<math.pow(2,size))
  }

  "create a node and check if it is added to ring properly" in {
    var list_of_names_to_Assign_to_node =  new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
    list_of_names_to_Assign_to_node.addOne("movie1")
    def MD5(s: String) = {
            MessageDigest.getInstance("MD5").digest(s.getBytes())
          }
          val system = ActorSystem("UserServerActors")
      val counter= 0
          val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
          node_Actors ! lookupdata.add_nodering(0.toString, 0.toString, counter, node_Actors.path, node_Actors.ref, 1, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys
        assert(lookupdata.actorspath.nonEmpty)
  }


  "check the value stored in the computing nodes" in {
    var list_of_names_to_Assign_to_node =  new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
    list_of_names_to_Assign_to_node.addOne("movie1")
    list_of_names_to_Assign_to_node.addOne("movie2")

    list_of_names_to_Assign_to_node.addOne("movie3")

    def MD5(s: String) = {
      MessageDigest.getInstance("MD5").digest(s.getBytes())
    }
    val system = ActorSystem("UserServerActors")
    val counter= 1
    val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
    node_Actors ! lookupdata.add_nodering(2.toString, 2.toString, counter, node_Actors.path, node_Actors.ref, 2, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys

      println(lookupdata.list_of_movies.size)
    assert(lookupdata.list_of_movies(0).equals("movie1 computing node"))
  }
}
