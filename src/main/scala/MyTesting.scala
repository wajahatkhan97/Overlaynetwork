
import java.security.MessageDigest

import akka.actor.TypedActor.{context, lookup, self}
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, ActorSystem, Props}
import com.sun.tools.javac.util.ListBuffer
import com.typesafe.config.ConfigFactory
import lookupdata.{create_fingertable, list_of_movies, map}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.math.BigDecimal.int2bigDecimal
import scala.util.control.Breaks.break
//https://medium.com/@TamasPolgar/what-to-do-with-5-000-000-akka-actors-381a915a0f78 "referenced for storing data in node actor(hashmap)"
object MyTesting extends App {

  var list_of_names_to_Assign_to_node =  new mutable.ListBuffer[String] //List("object1", "object2", "object3", "object4", "object5", "object6") //these values will be used as he objects
  var list_of_movies_Titles= new mutable.ListBuffer[String] //these will be the movies titles will be used as the key

    var key_actormap = new mutable.HashMap[Int,String]()
  case class UserActor(title: String, ref: ActorSelection, value: String,key_actorpath:mutable.HashMap[Int,String],numbernodes:Int)

  case class ServerActor(title: String)

  case class get_value(key: String) //get the value
  //case class add_node_to_ring(Key: String,ref:ActorRef ,path: ActorPath)
  var map = new mutable.HashMap[String, String]() //stores key with the value of actor reference
  val LOGGER = LoggerFactory.getLogger(classOf[Nothing])
  val server_Data = ConfigFactory.load("servernodes.conf").getConfig("server_data")
  val worker_Data = ConfigFactory.load("workernodes.conf").getConfig("worker_data")

  class akka extends Actor with ActorLogging {
    var n = 0;

    def receive = {
      case UserActor(title, ref, value,key_actorpath,numbernodes) =>

        ref ! lookupdata.Find_data(title, ref,key_actorpath,numbernodes)

    }

  }

  /*
  Approaching HomeWork 3
   */
  //Step 1: Implement the chord algorithm (first create only three nodes) with hashed key (still have to look)
  //step 2:  Create user actors and load data into servers
  //step 3: Then same user will request data from nodes using a hashed key
  val system = ActorSystem("UserServerActors")
//"movie1","movie2","movie3","movie4","movie5"
  //creating a hash value
  val number_nodes = server_Data.getInt("num-nodes") //val defines a constant value which cannot be modified once declared
  val user_actor = worker_Data.getInt("num-nodes")
  val size_of_movie_list = server_Data.getConfigList("movie-list").get(0).getString("size")
  //add the movie titles to buffered list
  for(counter <- 1 to size_of_movie_list.toInt) {
    list_of_movies_Titles.addOne(server_Data.getConfigList("movie-list").get(0).getString(counter.toString))
  }
  // add the movie object to buffered list

  for(counter <- 1 to size_of_movie_list.toInt) {
    list_of_names_to_Assign_to_node.addOne(server_Data.getConfigList("movie-objects").get(0).getString(counter.toString))
  }
  //create node_Actors
  def MD5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

  var counter = 0
  createserver()
  def createserver()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    for (counter <- 0 to number_nodes) {
      {
        Thread.sleep(1500) //1.5 sec delay

        val node_Actors = system.actorOf(Props[lookupdata], "nodeactor" + counter)
        //      println("Node created for the ring:   " + node_Actors.path)
        //create the ring here
        var hashValue_movietitle = MD5(list_of_movies_Titles(counter))
        var hashValue_movieobjects = MD5(list_of_names_to_Assign_to_node(counter))

        //  println("node:  " + counter + " Will be storing the hashed value: " + list_of_names_to_Assign_to_node(counter) + "  " + Math.abs((hashValue(counter)) % number_nodes).toString)
        key_actormap.addOne(counter, node_Actors.path.toString) // will be used to later to find the value

        node_Actors ! lookupdata.add_nodering(Math.abs(hashValue_movietitle(0) % number_nodes).toString, Math.abs(hashValue_movieobjects(counter)).toString, counter, node_Actors.path, node_Actors.ref, number_nodes, list_of_names_to_Assign_to_node(counter)) //have to add separate for hashed keys

      }
    }
  }
  //
    //reason for creating multiple actor is that each worker actor will look for different data.
    createuser()
    def createuser()= {
      LOGGER.info("Creating worker nodes to find data")

      for (counter <- 1 to user_actor) {
        Thread.sleep(2000)
        val user_actor = system.actorOf(Props[akka], "useractor" + counter)
        //for (counter <- 0 to number_nodes) {
        //  println(user_actor.path)
        val select_Actor = system.actorSelection(key_actormap.get(0).get) //getting the starting path to search the Ring
        var hashValue = MD5(list_of_movies_Titles(4))
        //this way we might produce multiple result for same value
        user_actor ! UserActor((Math.abs(hashValue(0)) % number_nodes).toString, select_Actor, list_of_names_to_Assign_to_node(counter), key_actormap, number_nodes) //loading the data into node actors via useractor
        //}
      }
    }
  system.terminate()
  }





