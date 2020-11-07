
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

  var list_of_names_to_Assign_to_node = List("Wajahat", "Umar", "Krunal", "CS441", "myname", "yourname") //these will be the values (after hashing them) that we are going to store in our Chord

    var key_actormap = new mutable.HashMap[Int,String]()
  case class UserActor(title: String, ref: ActorSelection, value: String,key_actorpath:mutable.HashMap[Int,String])

  case class ServerActor(title: String)

  case class Insert(key: String, value: String)

  case class get_value(key: String) //get the value
  case class insert(key: String, Value: String) //key value pair
  //case class add_node_to_ring(Key: String,ref:ActorRef ,path: ActorPath)
  var map = new mutable.HashMap[String, String]() //stores key with the value of actor reference


  class akka extends Actor with ActorLogging {
    var n = 0;

    def receive = {
      case UserActor(title, ref, value,key_actorpath) =>
        // println(self)
        //  println(title)

        //ref!ServerActor(title)
        ref ! lookupdata.Find_data(title, ref,key_actorpath)

    }


  }

  /*
  Approaching HomeWork 3
   */
  //Step 1: Implement the chord algorithm (first create only three nodes) with hashed key (still have to look)
  //step 2:  Create user actors and load data into servers
  //step 3: Then same user will request data from nodes using a hashed key
  val system = ActorSystem("UserServerActors")

  //creating a hash value
  val number_nodes = 4 //val defines a constant value which cannot be modified once declared
  val user_actor = 4

  //create node_Actors
  def MD5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

  var counter = 0

  for(counter <- 0 to 3) {
    {
      val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
      //      println("Node created for the ring:   " + node_Actors.path)
      //create the ring here
      var hashValue = MD5(list_of_names_to_Assign_to_node(counter))
      //so the finger table will consist of node identifiers and the keys will be stored in the finger table
      //Now from here we will start updating the finger table
      //so the chord use hashed value(nodeID but hashed + the formula) to determine a node designated by the hashed value
      //and when you want to look for data you can simply look for that hashed value.

    //  println("node:  " + counter + " Will be storing the hashed value: " + list_of_names_to_Assign_to_node(counter) + "  " + Math.abs((hashValue(counter)) % number_nodes).toString)
      key_actormap.addOne(counter, node_Actors.path.toString) // will be used to later to find the value

      node_Actors ! lookupdata.add_nodering(Math.abs(hashValue(counter)).toString, node_Actors.path, node_Actors.ref, number_nodes, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys

    }


    val user_actor = system.actorOf(Props[akka], "useractor" + counter)
    //for (counter <- 0 to number_nodes) {
    println(user_actor.path)
    //so the chord use hashed valu e(nodeID but hashed + the formula) to determine a node designated by the hashed value
    //and when you want to look for data you can simply look for that hashed value.
    val select_Actor = system.actorSelection("akka://UserServerActors/user/" + "nodeactor" + 0)
    var hashValue = MD5("Krunal")
   // println("Here too: " + Math.abs(hashValue(0) % number_nodes).toString)

    user_actor ! UserActor((Math.abs(hashValue(0))%number_nodes).toString, select_Actor, list_of_names_to_Assign_to_node(counter),key_actormap) //loading the data into node actors via useractor
  }


  }





