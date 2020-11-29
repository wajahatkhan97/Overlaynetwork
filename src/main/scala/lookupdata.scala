
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{LOGGER, MD5, akka, server_Data, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, actual_coordinate, actual_coordinate_zone, check_zone, coordinate_zone, count_nodes_in_bootstrapmap, create_zone, create_zone_bootstrap, find_zone, list, multiarray, print_Space, store_nodes, update_zones}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._

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
  var coordinate_zone = new mutable.HashMap[String,ListBuffer[Tuple2[Int,Int]]]()
  var actual_coordinate_zone = new mutable.HashMap[Tuple2[Int,Int],ListBuffer[Tuple2[Int,Int]]]()

  var idk_coordinate_zone = new mutable.HashMap[Tuple2[Int,Int],ListBuffer[Tuple2[Int,Int]]]()

  var actual_coordinate = new mutable.HashMap[String,ListBuffer[Int]]()

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
  var list1 = new ListBuffer[Int];
  var list = new ListBuffer[Tuple2[Int,Int]]; //contains the zones of each node actor including the bootstrap itself

  def find_zone(x:Int,y:Int)={

  }
  def check_zone(list:ListBuffer[Tuple2[Int,Int]],x:Int,y:Int,map:mutable.HashMap[Tuple2[Int,Int],ListBuffer[Tuple2[Int,Int]]]):Int={
   var count=0;
    if(map.size ==1){ //meaning we only have coordinates for bootstrap
      LOGGER.info("Just the bootstrap node simply return from here")
    }else {
      for (count <- 0 to 0) {

        map(list(count)).foreach(
          which_value => {
            if(which_value._1 < x && which_value._2 > x){
              LOGGER.info("YES") //so now I know that my new coordinates(point P) resides in which zone
            }
          }
        )
      }
    }
    return count;
  }
  def update_zones(list:ListBuffer[Tuple2[Int,Int]], zone:Int,map:mutable.HashMap[Tuple2[Int,Int],ListBuffer[Tuple2[Int,Int]]],x:Int,y:Int ):ListBuffer[Tuple2[Int,Int]]={
    var new_list = new ListBuffer[Tuple2[Int,Int]]; //contains the zones of each node actor including the bootstrap itself
    var count=0
    if(map.size ==1){ //meaning we only have coordinates for bootstrap
          LOGGER.info("Just the bootstrap node simply return from here")
            new_list.addOne(0,x)
          new_list.addOne(0,y)
          return new_list
    }
        else{
          LOGGER.info("Just the bootstrap node " + map(list(zone)).head)

      map(list(zone)).foreach(
        which_value => {
          if(which_value._1 < x && which_value._2 > x && count ==0){
            LOGGER.info("YES") //so now I know that my new coordinates(point P) resides in which zone
            count+1;
            new_list.addOne(which_value._1 ,x)
          }
          else{
            if(which_value._1 < y && which_value._2 > y && count ==1){
              LOGGER.info("YES") //so now I know that my new coordinates(point P) resides in which zone
              new_list.addOne(which_value._1 ,y)
              count+1;
            }
          }

        }
      )
      map.put(list(zone),new_list)
    //issue I'm having with this is that I'm unable to update my new node zones but update the zone of previous node
    }
    return new_list
  }

}

/*
CAN Implemetation approach
*/

class lookupdata extends Actor with ActorLogging {

//how about if we maintain a map for each coordinate and their zone limits

  def receive = {

    case create_zone_bootstrap(x,y,path)=>{
      var list1 = new ListBuffer[Tuple2[Int,Int]]; //contains the zones of each node actor including the bootstrap itself

      //LOGGER.info("Zone created")
        multiarray(x)(y) = 100 //originally we will store the key,value index here but for now store any random value
      /*
      so we have to pass a key and in the given index we will store a map<K,V>

      store all the nodes in a list and then randomly choose a number
      then use that number to get the index from the list so this way you will be accessing the current active node randomly

       For Split

       to check wether we should do a vertical or horizontal
       For vertical:
                We will check if the new node point is parallel to the current node (meaning if its not the same column then vertical split)
       For Horizontal:
                We will check if the new node point is below the current node (meaning if its in the same column then Horizontal split)

        Also, make sure to check we don't cross other nodes zone  (check their zones and compare with yours using the map that stores all the info)
       */
      LOGGER.info(Integer.toString(multiarray(x)(y)))
      list.addOne(x,y)

      coordinate_zone.put(path.toString,list)
      list1.addOne((0,x))
      list1.addOne((0,y)) //this list have the bootstrap node zone
      actual_coordinate_zone.put(list(count_nodes_in_bootstrapmap),list1) //this map will maintain the zones

      var zone=  check_zone(list,x,y,actual_coordinate_zone) //returns the zone

      //okay so we will have one more map whose key will be the coordinates and it contains tuples of zone limits
     var newlist= update_zones(list,zone,actual_coordinate_zone,x,y) //update the zone

      LOGGER.info("see" + actual_coordinate_zone)

      store_nodes.put(count_nodes_in_bootstrapmap,path.toString()) //store the nodes
      //LOGGER.info("So How many nodes we have now in the list returned by bootstrap: " + list )
      count_nodes_in_bootstrapmap+=1;

    }


    case create_zone(x,y) =>{
    // So, this part basically just notifies my bootstrap node about the newly created node

      val selection = system.actorSelection(store_nodes(0)); //this is bootstrap node //domain name basically resolves to the IP address
        selection!create_zone_bootstrap(x,y,self.path) //so self.path represents the newly created node
      //LOGGER.info("So How many nodes we have now in the list returned by bootstrap: " + store_nodes.size)

       var new_list= return_list() //returns the current active node map
        var random = scala.util.Random
          var range = random.nextInt(2)

    //  LOGGER.info("The generated random number is: " + new_list(0) + " " + range + "Coordinates "  + coordinate_zone(new_list(0)) + " " + coordinate_zone(new_list(1))  )
      //now randomly choose x,y points to join the coordinates
      //also the above returned map will help us in identifying the zone restriction
      //now iterate over each node in the list to check whose zone the coordinates lies in

    //  LOGGER.info("The coordinates are: " + x + " " + y )
      //before this you have to update the zones of each nodes

      find_zone(x,y) //sending current nodes points we have to find the zone based on this
    }

      def return_list():mutable.HashMap[Int,String]={
        return store_nodes //this will return the map that bootstrap node contains of all the active current active nodes.
        //random number generator will be work as key for this map
      }
    //compare the randomly generated point p and find which zone it resides in by going over the coordinateS_zone map and then return the path of that particular node

    case Find_data(key, ref, keyactor_path, numbernodes) => {

    }

  }
}