
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{LOGGER, MD5, akka, server_Data, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, actual_coordinate, actual_coordinate_zone, check_update_zone, coordinate_list, coordinate_zone, count_nodes_in_bootstrapmap, create_zone, create_zone_bootstrap, find_zone, list, multiarray, print_Space, store_nodes, update_zones}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._
import scala.util.control.Breaks.break

/*
//Chord Implementation idea was from "Mr.Abhijeet"  .
*/
object lookupdata {

  case class Find_data(key: String, ref: ActorSelection, key_Actor: mutable.HashMap[Int, String], numbernodes: Int) //key value pair
  case class create_zone_bootstrap(x: Int, y: Int, path: ActorPath)

  case class create_zone(x: Int, y: Int)

  var x = 4; //width
  var y = 4; //height
  var count_nodes_in_bootstrapmap = 0
  var coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()
  var actual_coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()

  var idk_coordinate_zone = new mutable.HashMap[Tuple2[Int, Int], ListBuffer[Tuple2[Int, Int]]]()

  var actual_coordinate = new mutable.HashMap[String, ListBuffer[Int]]()

  var store_nodes = new mutable.HashMap[Int, String]()

  //2-D ARRAY
  val multiarray = Array.ofDim[Int](5, 5)
  for (i <- 0 to 4) {
    for (j <- 0 to 4) {
      multiarray(i)(j) = 0
    }
  }

  def print_Space() = {
    for (i <- 0 to 4) {
      for (j <- 0 to 4) {
        {
          print(" " + multiarray(i)(j) + " ")
        }
      }
      println()
    }
  }

  var coordinate_list = new ListBuffer[Int];
  var list = new ListBuffer[Tuple2[Int, Int]]; //contains the zones of each node actor including the bootstrap itself

  def find_zone(x: Int, y: Int) = {

  }

  def check_update_zone(x:Int,y:Int,store_nodes:mutable.HashMap[Int, String],actual_Cordinates:mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]) = {
    /*
    So x represents the new nodes X point and Y represents the Y point
    store_nodes have the path of each node via which
    actual coordinates have the actual zones where node path is the key
     -----implementation-------
        check which path contains the zone return the path and in update zone update the map using the
    //TODO: STORE KEY VALUE AT THE INDEX
     */

    var previous_x_coordinate = 0;
    var previous_y_coordinate = 0;
    var x_coordinate =false;
    var y_coordinate =false;
    var list_new = new ListBuffer[Tuple2[Int, Int]]; //contains the zones of each node actor including the bootstrap itself


    for(count<-0 to 1)
      {
        var counts =0

        actual_Cordinates(store_nodes(count)).foreach(

          value=>{

            if(counts==0) {
              if(value._1 < x && x< value._2  ){  //z<x<g
                LOGGER.info("The data is:  " + actual_Cordinates(store_nodes(count)))
                x_coordinate=true;
                previous_x_coordinate = value._2
              }
            }else{
              //this is the Y coordinate zone
              if(value._1 < y && y < value._2){
                LOGGER.info("The data is:  " + actual_Cordinates(store_nodes(count)))
                y_coordinate=true;
                previous_y_coordinate = value._2

              }
              }
            counts+=1
            if(x_coordinate&&y_coordinate){
              //update the map of index
                list_new.addOne(value._1,y)
              list_new.addOne(value._1,y)
              actual_Cordinates.put(store_nodes(count),list_new)
              x_coordinate=false
              y_coordinate=false
            }
          }
        )
      }
          //now update the current on
    list_new = new ListBuffer[Tuple2[Int, Int]];
    var this_count=0
    actual_Cordinates(store_nodes(1)).foreach(
      value=>{
                if(this_count==0) {
                  list_new.addOne(value._2, previous_x_coordinate)
                }else{
                  if(this_count==1) {
                    list_new.addOne(0, previous_y_coordinate)//lower bound?
                  }
                }
        this_count+=1
           /// list_new.addOne(value._2,previous_y_coordinate)

      }
    )
          actual_Cordinates.put(store_nodes(1),list_new)
        }



  def update_zones() = {

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
      var coordinate_list = new ListBuffer[Int];

      //LOGGER.info("Zone created")
        multiarray(x)(y) = 100 //originally we will store the key,value index here but for now store any random value
      /*
      so we have to pass a key and in the given index we will store a map<K,V>

      store all the nodes in a list and then randomly choose a number
      then use that number to get the index from the list so this way you will be accessing the current active node randomly

       For Split

       to check wether we should do a vertical or horizontal
      TODO: For vertical:
                We will check if the new node point is parallel to the current node (meaning if its not the same column then vertical split)
       For Horizontal:
                We will check if the new node point is below the current node (meaning if its in the same column then Horizontal split)
                Creating routing table "KEEP TRACK OF NEIGHBOURS"

        Also, make sure to check we don't cross other nodes zone  (check their zones and compare with yours using the map that stores all the info)
       */
      LOGGER.info(Integer.toString(multiarray(x)(y)))
      if(count_nodes_in_bootstrapmap==0) { //just only once
        list.addOne(0, 4) //4x4 x
        list.addOne(0, 4)//y
        coordinate_list.addOne(x)
        coordinate_list.addOne(y)

        actual_coordinate.put(path.toString,coordinate_list)
        actual_coordinate_zone.put(path.toString, list) //this list will be the one to get updated
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes

      }
      if(count_nodes_in_bootstrapmap>0) {
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes

        list1.addOne((0, x))
        list1.addOne((0, y)) //this list have the bootstrap node zone
        //In check zone where this current coordinates lands
        coordinate_list.addOne(x)
        coordinate_list.addOne(y)

        actual_coordinate.put(path.toString,coordinate_list)
        actual_coordinate_zone.put(path.toString, list1) //this list will be the one to get updated

        check_update_zone(x,y,store_nodes,actual_coordinate_zone) //returns the zone



        LOGGER.info("Bootstrap" + coordinate_zone)
        LOGGER.info("Nodes" + actual_coordinate_zone)
        LOGGER.info("Actual coordinate" + actual_coordinate)

      }
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
