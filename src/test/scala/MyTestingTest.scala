import java.security.MessageDigest

import _root_.akka.actor.{ActorSystem, Props}
import _root_.akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import _root_.akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import _root_.akka.testkit.{TestProbe, filterEvents}
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class MyTestingTest extends ScalaTestWithActorTestKit with AnyWordSpecLike {
  //create the user actor and check if its returning the correct value

//
//  "Check to see if keys are generating properly" in {
//    var size = 6;
//    def MD5(s: String) = {
//      MessageDigest.getInstance("MD5").digest(s.getBytes())
//    }
//
//    val title = "movie1"
//    var hashValue_movietitle = MD5(title)
//
//    assert(hashValue_movietitle(0) < math.pow(2, size))
//  }
//
//  "create a node and check if it is added to ring properly" in {
//    var list_of_names_to_Assign_to_node = new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
//    list_of_names_to_Assign_to_node.addOne("movie1")
//
//    val system = ActorSystem("UserServerActors")
//    val counter = 0
//    val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
//    node_Actors ! lookupdata.add_nodering(0.toString, 0.toString, counter, node_Actors.path, node_Actors.ref, 1, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys
//    assert(lookupdata.actorspath.isEmpty)
//  }
//
//
//  "check the value stored in the computing nodes" in {
//    var list_of_names_to_Assign_to_node = new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
//    list_of_names_to_Assign_to_node.addOne("movie1")
//    list_of_names_to_Assign_to_node.addOne("movie2")
//
//    list_of_names_to_Assign_to_node.addOne("movie3")
//
//    def MD5(s: String) = {
//      MessageDigest.getInstance("MD5").digest(s.getBytes())
//    }
//
//    val system = ActorSystem("UserServerActors")
//    val counter = 1
//    val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
//    node_Actors ! lookupdata.add_nodering(2.toString, 2.toString, counter, node_Actors.path, node_Actors.ref, 2, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys
//
//    println(lookupdata.list_of_movies.size)
//    assert(lookupdata.list_of_movies(0).equals("movie1 computing node"))
//  }
//
//
//  "create multiple nodes and store values" in {
//    var list_of_names_to_Assign_to_node = new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
//    list_of_names_to_Assign_to_node.addOne("object1")
//    list_of_names_to_Assign_to_node.addOne("object2")
//    list_of_names_to_Assign_to_node.addOne("object3")
//    var list_of_movies_Titles = new ListBuffer[String]
//    list_of_movies_Titles.addOne("movie1")
//    list_of_movies_Titles.addOne("movie2")
//    list_of_movies_Titles.addOne("movie3")
//
//    def MD5(s: String) = {
//      MessageDigest.getInstance("MD5").digest(s.getBytes())
//    }
//
//    val system = ActorSystem("UserServerActors")
//    val counter = 0
//    for (counter <- 0 to 1) {
//
//      val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
//      var hashValue_movietitle = MD5(list_of_movies_Titles(counter))
//      var hashValue_movieobjects = MD5(list_of_names_to_Assign_to_node(counter))
//      node_Actors ! lookupdata.add_nodering(Math.abs(hashValue_movietitle(0) % 2).toString, Math.abs(hashValue_movieobjects(counter)).toString, counter, node_Actors.path, node_Actors.ref, 2, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys
//
//    }
//    assert(lookupdata.actorspath.nonEmpty)
//  }
//
//
//  "creating both node actor and user actor" in {
//    var list_of_names_to_Assign_to_node = new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
//    list_of_names_to_Assign_to_node.addOne("movie1")
//    list_of_names_to_Assign_to_node.addOne("movie2")
//
//    list_of_names_to_Assign_to_node.addOne("movie3")
//    var key_actormap = new mutable.HashMap[Int,String]()
//
//    def MD5(s: String) = {
//      MessageDigest.getInstance("MD5").digest(s.getBytes())
//    }
//
//    val system = ActorSystem("UserServerActors")
//    val counter = 1
//    val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
//    key_actormap.put(0,node_Actors.path.toString)
//    node_Actors ! lookupdata.add_nodering(2.toString, 2.toString, counter, node_Actors.path, node_Actors.ref, 2, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys
//
//    Thread.sleep(2000)
//    val user_actor = system.actorOf(Props[akka], "useractor" + counter)
//    //for (counter <- 0 to number_nodes) {
//    //  println(user_actor.path)
//    val select_Actor = system.actorSelection(key_actormap.get(0).get) //getting the starting path to search the Ring
//    var hashValue = MD5(list_of_names_to_Assign_to_node(1))
//    //this way we might produce multiple result for same value
//    user_actor ! UserActor((Math.abs(hashValue(0)) % 1).toString, select_Actor, list_of_names_to_Assign_to_node(counter), key_actormap, 1) //loading the data into node actors via useractor
//    //}
//    assert(lookupdata.store_nodenumbers.size>0)
//
//  }
}
