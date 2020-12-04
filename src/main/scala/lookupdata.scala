import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{LOGGER, MD5, akka, all_nodes_paths, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, actual_coordinate, actual_coordinate_zone, check_update_zone, coordinate_list, coordinate_zone, copy_routing, count_nodes_in_bootstrapmap, create_zone, create_zone_bootstrap, find_zone, i, list, multiarray, print_Space, routing_table, routing_table_global, store_nodes, update_zones}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._
import scala.util.control.Breaks.break

/*
//CAN Implementation .
TODO: Node Failures/departure
      Data Finding
*/
object lookupdata {

  case class Find_data(key: String, ref: ActorSelection, key_Actor: mutable.HashMap[Int, String], numbernodes: Int) //key value pair
  case class create_zone_bootstrap(x: Int, y: Int, path: ActorPath)
  case class create_zone(x: Int, y: Int)
  case class routing_table(coordinates:Tuple2[Int,Int],Path:String)
  case class copy_routing(coordinates:Tuple2[Int,Int],path:String)
  var x = 4; //width
  var y = 4; //height
  var i=0
  var count_nodes_in_bootstrapmap = 0
  var coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()
  var actual_coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()

  var idk_coordinate_zone = new mutable.HashMap[Tuple2[Int, Int], ListBuffer[Tuple2[Int, Int]]]()

  var actual_coordinate = new mutable.HashMap[String, ListBuffer[Tuple2[Int,Int]]]()
  var routing_table_global = new mutable.HashMap[Tuple2[Int,Int],String]()
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

  def check_update_zone(x:Int,y:Int,storenodes:mutable.HashMap[Int, String],path:String):String = {
    //TODO: STORE KEY VALUE AT THE INDEX
    var list = new ListBuffer[Tuple2[Int, Int]]; //contains the zones of each node actor including the bootstrap itself
    //This is the case where we have only one Node in  the Grid
    var x_coordinate = false
    var y_coordinate = false
    var prev_lower_bound_X = 0;
    var prev_lower_bound_y = 0;


    var lower_bound_X = 0;
    var lower_bound_y = 0
    var upper_bound_x = 0
    var upper_bound_y = 0
    var count = 0
    if (storenodes.size == 1) {
      LOGGER.info("Okay this is the case of Bootstrap_node")
      list.addOne(0, multiarray.length-1)
      list.addOne(0, multiarray.length-1)
      actual_coordinate_zone.put(storenodes(0), list)
    } else {
      for (i <- 0 until storenodes.size-1) {

        actual_coordinate_zone(storenodes(i)).foreach(
          value => {
            if (count % 2 == 0) {
              if (value._1 <= x && value._2 >= x) {
                x_coordinate = true
                prev_lower_bound_X = value._1
                lower_bound_X = value._1
                upper_bound_x = value._2
              }
            }
            else {
              if (value._1 <= y && value._2 >= y) {
                y_coordinate = true
                prev_lower_bound_y=value._1
                lower_bound_y = value._1
                upper_bound_y = value._2

              }
            }
            if(x_coordinate&&y_coordinate&&prev_lower_bound_X==x){
              LOGGER.info("do the horizontal split")
              lower_bound_y = (lower_bound_y+upper_bound_y)/2
              list.addOne(lower_bound_X,upper_bound_x)
              list.addOne(prev_lower_bound_y,lower_bound_y)
              actual_coordinate_zone.put(storenodes(i),list)
              update_zones_horizontal(prev_lower_bound_X,lower_bound_X,lower_bound_y,upper_bound_y,path,prev_lower_bound_y) //here path is basically the path of the newly created node

              x_coordinate = false
              y_coordinate = false

              return  (storenodes(i)) //return the path that
            }
            if (x_coordinate && y_coordinate) {
              //split the zone in half
              lower_bound_X = (lower_bound_X + upper_bound_x) / 2 //the upper bound for this coordinate will be the lower bound for next
              list.addOne(lower_bound_X+1, upper_bound_x)
              list.addOne(lower_bound_y, upper_bound_y)
              actual_coordinate_zone.put(storenodes(i),list)
              update_zones(prev_lower_bound_X,lower_bound_X,lower_bound_y,upper_bound_y,path) //here path is basically the path of the newly created node
              x_coordinate = false
              y_coordinate = false

              return  (storenodes(i)) //return the path that

            }
            count += 1
          }
        )
      }
    }
    return "nothing"


  }



  def update_zones_horizontal(lower_bound:Int , upper_bound:Int, lower_bound_y:Int,upper_bound_y:Int,path:String,prev_lowerbound:Int) = {
    var list = new ListBuffer[Tuple2[Int, Int]]; //contains the zones of each node actor including the bootstrap itself

    list.addOne(lower_bound,upper_bound)
    list.addOne(lower_bound_y,upper_bound_y)
    actual_coordinate_zone.put(path,list)

  }

  def update_zones(lower_bound:Int , upper_bound:Int, lower_bound_y:Int,upper_bound_y:Int,path:String) = {
    var list = new ListBuffer[Tuple2[Int, Int]]; //contains the zones of each node actor including the bootstrap itself

    list.addOne(lower_bound,upper_bound)
    list.addOne(lower_bound_y,upper_bound_y)
    actual_coordinate_zone.put(path,list)

  }

}
/*
CAN Implemetation approach
*/

class lookupdata extends Actor with ActorLogging {

  //how about if we maintain a map for each coordinate and their zone limits
  var routing_table1 = new mutable.HashMap[Tuple2[Int,Int],String]()



  def receive = {

    case copy_routing(coordinates,path)=>{
      routing_table_global.put(coordinates,path) //
      LOGGER.info("routing table for global: " + routing_table_global) //helper map
      LOGGER.info("routing table for nodes: " + path + routing_table1)

    }
    case routing_table(coordinates,path)=>{
      //few things need to be fix here
     // LOGGER.info("Updating the neighbours")
      if(store_nodes.size==1){
        routing_table_global.put(coordinates,path) //this is global

        routing_table1.put(coordinates,path)//this is resticted to specific nodes
      }
      else{
        //check left,right,up,down
        if(routing_table_global.contains((coordinates._1-1),coordinates._2)){
//          LOGGER.info("There is a neighbour left")
//          LOGGER.info(self.path.toString)

          var neighbour_path = system.actorSelection(routing_table_global((coordinates._1-1),coordinates._2))
          routing_table1.put(coordinates,path)
          routing_table1.put((coordinates._1-1,coordinates._2),neighbour_path.pathString)
          neighbour_path!copy_routing(coordinates,path)

        }
        if(routing_table_global.contains((coordinates._1+1),coordinates._2)){
//          LOGGER.info("There is a neighbour Right")
//          LOGGER.info(self.path.toString)

          var neighbour_path = system.actorSelection(routing_table_global((coordinates._1+1),coordinates._2))

          routing_table1.put(coordinates,path)
          routing_table1.put((coordinates._1+1,coordinates._2),neighbour_path.pathString)

          neighbour_path!copy_routing(coordinates,path)

        }
        if(routing_table_global.contains((coordinates._1),(coordinates._2-1))){
//          LOGGER.info("There is a neighbour up")
//          LOGGER.info(self.path.toString)

          var neighbour_path = system.actorSelection(routing_table_global((coordinates._1),coordinates._2-1))
          routing_table1.put(coordinates,path)
          routing_table1.put((coordinates._1,coordinates._2-1),neighbour_path.pathString)
          neighbour_path!copy_routing(coordinates,path)

        }
        if(routing_table_global.contains((coordinates._1),(coordinates._2+1))){
//          LOGGER.info("There is a neighbour down")
//          LOGGER.info(self.path.toString)

          var neighbour_path = system.actorSelection(routing_table_global((coordinates._1),coordinates._2+1))
          routing_table1.put(coordinates,path)
          routing_table1.put((coordinates._1,coordinates._2+1),neighbour_path.pathString)
          neighbour_path!copy_routing(coordinates,path)

        }


      }
    }

    case create_zone_bootstrap(x,y,path)=>{
      var list1 = new ListBuffer[Tuple2[Int,Int]]; //contains the zones of each node actor including the bootstrap itself
      var coordinate_list = new ListBuffer[Int];
      var actual_pair = new mutable.HashMap[Int,Int]() //where string represents the path of node and key_value pair
      var store_pair = new mutable.HashMap[String,mutable.HashMap[Int,Int]]() //where string represents the path of node and key_value pair
      //  TODO: store the (key,value) pair with the path
      multiarray(x)(y) = 100 //originally we will store the key,value index here but for now store any random value
      //actual_pair.put(key,value);
      //store_pair.put(path,actual_pair)


      LOGGER.info(Integer.toString(multiarray(x)(y)))
      if(count_nodes_in_bootstrapmap==0) { //just only once
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes
        list1.addOne(x,y)
        actual_coordinate.put(path.toString,list1)
        check_update_zone(x,y,store_nodes,path.toString)
        self ! routing_table((x,y),path.toString)
      }
      if(count_nodes_in_bootstrapmap>0) {
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes
        list1.addOne(x,y)
        actual_coordinate.put(path.toString,list1)
        check_update_zone(x,y,store_nodes,path.toString)
        var testing = system.actorSelection(path)
        testing ! routing_table((x,y),path.toString) //update the neighbours
      }
      count_nodes_in_bootstrapmap+=1;

    }


    case create_zone(x,y) =>{
      val selection = system.actorSelection(store_nodes(0)); //this is bootstrap node //domain name basically resolves to the IP address
      selection!create_zone_bootstrap(x,y,self.path) //so self.path represents the newly created node

      var new_list= return_list() //returns the current active node map
      var random = scala.util.Random
      var range = random.nextInt(2)


      //  LOGGER.info("The generated random number is: " + new_list(0) + " " + range + "Coordinates "  + coordinate_zone(new_list(0)) + " " + coordinate_zone(new_list(1))  )
      //now randomly choose x,y points to join the coordinates
      //also the above returned map will help us in identifying the zone restriction

      find_zone(x,y) //sending current nodes points we have to find the zone based on this
    }

      def return_list():mutable.HashMap[Int,String]={
        return store_nodes //this will return the map that bootstrap node contains of all the active current active nodes.
        //random number generator will be work as key for this map
      }
      //compare the randomly generated point p and find which zone it resides in by going over the coordinateS_zone map and then return the path of that particular node

//      def return_routingtable():mutable.HashMap[Tuple2[Int,Int],String]={
////        routing_table_global = routing_table1
////        return routing_table1
//      }
//Note: Routing table fixed
    case Find_data(key, ref, keyactor_path, numbernodes) => {
      /*
      so for finding data we will first find the coordinates via the coordinates we will find the path and with the path we will retrieve the
      key value pairs

       */



    }

  }
}