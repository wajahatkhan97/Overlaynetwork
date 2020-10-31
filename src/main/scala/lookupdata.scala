import akka.actor.{Actor, Props}
import MyTesting.{MD5, UserActor, akka, list_of_names_to_Assign_to_node, map, system}
import lookupdata.{Insert, getValue}

import scala.collection.mutable
object lookupdata{
  var map = new mutable.HashMap[String,String]()
  case class getValue(Key:String)
  case class Insert(key:String , Value:String) //key value pair

   }

class lookupdata extends Actor{

  def receive ={
    case Insert(key,value) =>{
      map.addOne(key,value)
      println("Inside the hmap function:  " + self + "    " + map.get(key))
    }
    case getValue(key) =>{
      if(map.contains(key)){
        println(map.get(key))
      }
      else {
        println("No key found")
      }
    }

  }

  }

