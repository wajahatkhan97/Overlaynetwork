import java.security.MessageDigest

import akka.actor.{Actor, ActorRef, ActorSelection, ActorSystem, Props}
import lookupdata.{map,getValue}
import scala.collection.mutable
import scala.util.control.Breaks.break
//https://medium.com/@TamasPolgar/what-to-do-with-5-000-000-akka-actors-381a915a0f78 "referenced for storing data in node actor(hashmap)"
object MyTesting extends App{

  var list_of_names_to_Assign_to_node = List("Wajahat","Umar","Krunal","CS441","myname","yourname") //fake data stored in nodes actors


  case class UserActor(title:String, ref:ActorSelection,value:String)
  case class ServerActor(title:String)
  case class Insert(key:String,value: String)
  case class get_value (key:String) //get the value
  case class insert(key:String , Value:String) //key value pair

  var map = new mutable.HashMap[String,String]() //stores key with the value of actor reference

  class hmap extends Actor{
    def receive ={
      case insert(key,value)=>{
        map.addOne(key,value)
        println("Inside the hmap function:  " + self + "    " + map.get(key))
        lookupdata.map = map
      }
    }
  }

//  def getvalue(Key:String): Unit ={
//    if(map.contains(Key)){
//      println(map.get(Key))
//    }
//    else {
//      println("No key found")
//    }
//  }

  class akkanode extends Actor{
    def receive ={
      case ServerActor(title) =>
        println(self)
        println(title)
    }
  }
  class akka extends Actor{
    var n=0;
    def receive={
      case UserActor(title,ref,value) =>
        // println(self)
        //  println(title)

        //ref!ServerActor(title)
        ref!lookupdata.Insert(title,value)
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
  def MD5(s:String)={MessageDigest.getInstance("MD5").digest(s.getBytes())}
  var counter=0
  for(counter <- 1 to number_nodes) {
    {
      val node_Actors = system.actorOf(Props[lookupdata],"nodeactor"+counter)
      println("Node created for the ring:   " + node_Actors.path)

     // println("The key generated for the given node:  " + hashValue)
    }

  }

  for(counter <- 1 to number_nodes) {

    val user_actor = system.actorOf(Props[akka],"useractor"+counter)
    val select_Actor = system.actorSelection("akka://UserServerActors/user/"+"nodeactor"+counter)
    var to_behashed = counter.toString;
    var hashValue=MD5("nodeactor"+to_behashed)
    user_actor ! UserActor(counter.toString,select_Actor,list_of_names_to_Assign_to_node(counter)) //loading the data into node actors via useractor
  }

  for(counter <- 1 to number_nodes) {

    val select_Actor = system.actorSelection("akka://UserServerActors/user/"+"nodeactor"+counter)
    var to_behashed = counter.toString;
    var hashValue=MD5("nodeactor"+to_behashed)
      select_Actor ! lookupdata.getValue(counter.toString)
  }

}
