import java.lang.System.Logger

import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, Props}
import MyTesting.{MD5, UserActor, akka, list_of_names_to_Assign_to_node, map, system}
import lookupdata.{Insert, add_nodering, create_fingertable, getValue}

import scala.collection.mutable
/*
//Chord Implementation idea idea was taken from  "Mr.Abhijeet's " github repo.

*/
object lookupdata{
  case class getValue(Key:String)
  case class Insert(key:String , Value:String) //key value pair
  var map = new mutable.HashMap[String,String]()
  case class find_successor() ////the hashed key user request for will be looked into each of the finger_table and data will be retrieved from there
  case class find_predecessor()
  case class update_finger_table() //updating finger table comes in when a new node joins so that you update in the current node
  case class create_fingertable(totalnodes:Int) //finger table will contains the immediate successors (atleast 2) of our current node
  var num_of_nodes_in_ring=0;
  case class add_nodering(Key:String,actorPath: ActorPath,ref: ActorRef)
}

class lookupdata extends Actor with ActorLogging {
  var actorspath = new mutable.ListBuffer[String] //List Buffer gives constant time
  var lookup_servernodes = new mutable.HashMap[String,String]
  def receive ={
    case create_fingertable(totalnodes) =>{
      //so our approach is basically we will create finger table for each node
      //we also update the existing one(s) all the previous nodes created (node joining requires to keep track of the predecessor but we are
      // keeping track of all paths and with them we are retrieving finger tables and updating them
      // (this part was required in Project but we are doing it here)
        println("Generating finger table for Node: " + self.path)

      //generate finger table using the formula ("remember table contains the identifier and the key
    }
    case add_nodering(key,actorPath,ref) =>{
      actorspath.addOne(actorPath.toString)
      lookup_servernodes.addOne(key->actorPath.toString) //storing server path with key to access (while getting the data)
      log.info("Node added to the ring")
      ref!create_fingertable(4)

    }

    case Insert(key,value) =>{
      map.addOne(key,value)
      println("Inside the hmap function:  " + self + "    " + map.get(key))
    }
    case getValue(key) =>{
      if(map.contains(key)){
        println("CONTAINS:" + map.get(key))
      }
      else {
        println("No key found")
      }
    }

  }

  }

