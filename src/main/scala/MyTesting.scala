
import java.security.MessageDigest

import akka.actor.TypedActor.{context, lookup, self}
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, ActorSystem, Props}
import lookupdata.{getValue, map}
import lookupdata.create_fingertable

import scala.collection.mutable
import scala.util.control.Breaks.break
//https://medium.com/@TamasPolgar/what-to-do-with-5-000-000-akka-actors-381a915a0f78 "referenced for storing data in node actor(hashmap)"
object MyTesting extends App {

  var list_of_names_to_Assign_to_node = List("Wajahat", "Umar", "Krunal", "CS441", "myname", "yourname") //fake data stored in nodes actors


  case class UserActor(title: String, ref: ActorSelection, value: String)

  case class ServerActor(title: String)

  case class Insert(key: String, value: String)

  case class get_value(key: String) //get the value
  case class insert(key: String, Value: String) //key value pair
  //case class add_node_to_ring(Key: String,ref:ActorRef ,path: ActorPath)
  var map = new mutable.HashMap[String, String]() //stores key with the value of actor reference


  class akka extends Actor with ActorLogging {
    var n = 0;

    def receive = {
      case UserActor(title, ref, value) =>
        // println(self)
        //  println(title)

        //ref!ServerActor(title)
        ref ! lookupdata.Insert(title, value)

    }


  }


  /*
  Approaching HomeWork 3
   */
  //Step 1: Implement the chord algorithm (first create only three nodes) with hashed key (still have to look)
  //step 2:  Create user actors and load data into servers
  //step 3: Then same user will request data from nodes using a hashed key
  val system = ActorSystem("UserServerActors")
  //  val userActor = system.actorOf(Props[akka],"UserActor")
  //  val serverActor = system.actorOf(Props[akka],"ServerActor")
  //  userActor!UserActor("Hello Server",serverActor)

  //creating a hash value
  val number_nodes = 4 //val defines a constant value which cannot be modified once declared
  val user_actor = 4

  //create node_Actors
  def MD5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

  var counter = 0

  //for(counter <- 0 to 1)
  {
    {
      val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + 0)
      //      println("Node created for the ring:   " + node_Actors.path)
      //create the ring here
      var to_behashed = counter.toString;
      var hashValue = MD5("nodeactor" + to_behashed)
      //so the finger table will consist of node identifiers and the keys will be stored in the finger table
      //Now from here we will start updating the finger table
      node_Actors ! lookupdata.add_nodering(counter.toString, node_Actors.path, node_Actors.ref)
      val node_Actors1 = system.actorOf(Props[lookupdata], "nodeactor" + 1)
      var to_behashed1 = counter + 1.toString;
      var hashValue1 = MD5("nodeactor" + to_behashed1)

      node_Actors1 ! lookupdata.add_nodering(counter + 1.toString, node_Actors1.path, node_Actors1.ref)
    }


    val user_actor = system.actorOf(Props[akka], "useractor" + counter)
       for (counter <- 0 to number_nodes) {

      val select_Actor = system.actorSelection("akka://UserServerActors/user/" + "nodeactor" + 0)
      var to_behashed = counter.toString;
      var hashValue = MD5("nodeactor" + to_behashed)
      user_actor ! UserActor(counter.toString, select_Actor, list_of_names_to_Assign_to_node(counter)) //loading the data into node actors via useractor

    }

  }
}
//
//  for(counter <- 1 to number_nodes) {
//
//    val select_Actor = system.actorSelection("akka://UserServerActors/user/"+"nodeactor"+counter)
//    //var to_behashed = counter.toString;
//    //var hashValue=MD5(to_behashed)
//    select_Actor ! lookupdata.getValue(counter.toString)
//
//  }



