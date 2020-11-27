
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{LOGGER, MD5, akka, server_Data, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, coordinate_zone, count_nodes_in_bootstrapmap, create_zone, create_zone_bootstrap, multiarray, print_Space, store_nodes}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
//Chord Implementation idea was from "Mr.Abhijeet"  .
*/
object lookupdata{
  case class Find_data(key:String , ref:ActorSelection,key_Actor:mutable.HashMap[Int,String],numbernodes:Int) //key value pair
  case class create_zone_bootstrap(x:Int,y:Int,path:ActorPath)
  case class create_zone(x:Int,y:Int)

  var x = 10; //width
  var y = 10; //height
  var count_nodes_in_bootstrapmap=0
  var coordinate_zone = new mutable.HashMap[String,ListBuffer[Int]]()
  var store_nodes = new mutable.HashMap[Int,String]()

  //2-D ARRAY
  val multiarray = Array.ofDim[Int](5,5)
  for(i<-0 to 4)
    {
      for(j<-0 to 4)
        {
          multiarray(i)(j)= 0
        }
    }
  def print_Space()= {
    for(i<-0 to 4)
    {
      for(j<-0 to 4) {
        {
          print(" " + multiarray(i)(j) + " ")
        }
      }
      println()
    }
  }
}

/*
CAN Implemetation approach

create a rectangle with bootstrap node
currently the bootstrap node posses entire space lets say some node wants to
you will split the zone depending on the X value of the joining node

 */



class lookupdata extends Actor with ActorLogging {

//how about if we maintain a map for each coordinate and their zone limits

  var list = new ListBuffer[Int];
  def receive = {

    case create_zone_bootstrap(x,y,path)=>{

      LOGGER.info("Zone created")
        multiarray(x)(y) = 100 //originally we will store the key,value index here but for now store any random value
      /*
      so we have to pass a key and in the given index we will store a map<K,V>

      store all the nodes in a list and then randomly choose a number
      then use that number to get the index from the list so this way you will be accessing the current active node randomly
       */
      LOGGER.info(Integer.toString(multiarray(x)(y)))
      list.addOne(5)
      list.addOne(5)
      coordinate_zone.put("BootstrapNode",list)
        store_nodes.put(count_nodes_in_bootstrapmap,path.toString()) //store the nodes
      LOGGER.info("So How many nodes we have now in the list returned by bootstrap: " + store_nodes)
      count_nodes_in_bootstrapmap+=1;

    }

    case create_zone(x,y) =>{

    // So, this part basically just notifies my bootstrap node about the newly created node
      val selection = system.actorSelection(store_nodes(0)); //this is bootstrap node //domain name basically resolves to the IP address
        selection!create_zone_bootstrap(0,0,self.path) //so self.path represents the newly created node
      LOGGER.info("So How many nodes we have now in the list returned by bootstrap: " + store_nodes.size)
    }

    case Find_data(key, ref, keyactor_path, numbernodes) => {

    }

  }
}