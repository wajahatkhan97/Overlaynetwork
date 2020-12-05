import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{LOGGER, MD5, akka, all_nodes_paths, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, actual_coordinate, actual_coordinate_zone, check_update_zone, coordinate_list, coordinate_zone, copy_routing, count_nodes_in_bootstrapmap, create_zone, create_zone_bootstrap, distance, find_zone, i, list, multiarray, print_Space,  routing_table, routing_table_global, store_nodes, store_pair, update_routing, update_zones}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._
import scala.math.{pow, sqrt}
import scala.util.control.Breaks.break

/*
//CAN Implementation .
TODO: Node Failures/departure
      Data Finding
*/
object lookupdata {

  case class Find_data(coordinates:Tuple2[Int,Int],key:Int) //key value pair
  case class create_zone_bootstrap(x: Int, y: Int, path: ActorPath)
  case class create_zone(x: Int, y: Int,key:Int,Value:Int)
  case class routing_table(coordinates:Tuple2[Int,Int],Path:String)
  case class copy_routing(coordinates:Tuple2[Int,Int],path:String)
  var x = 4; //width
  var y = 4; //height
  var i=0
  var store_pair = new mutable.HashMap[String,mutable.HashMap[Int,Int]]() //where string represents the path of node and key_value pair

  var count_nodes_in_bootstrapmap = 0
  var coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()
  var actual_coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()
  var idk_coordinate_zone = new mutable.HashMap[Tuple2[Int, Int], ListBuffer[Tuple2[Int, Int]]]()

  var actual_coordinate = new mutable.HashMap[String, Tuple2[Int,Int]]()
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

  def update_routing()={
      store_nodes.foreach(
        value =>{
          LOGGER.info("Path " + value._2 + " " + lookupdata.actual_coordinate(value._2))
        }
      )
  }
  def distance(tuple2: Tuple2[Int,Int]):Tuple2[Int,Int]={

    var list = new ListBuffer[Double]
      //(x2-x1)+(y2-y1)
    //right
   var right= sqrt(pow(tuple2._1+1 -tuple2._1, 2) + pow(tuple2._2 - tuple2._2, 2))
    //left
    var left = sqrt(pow(tuple2._1-1 -tuple2._1, 2) + pow(tuple2._2 - tuple2._2, 2))
    //up
    var up = sqrt(pow(tuple2._1 -tuple2._1, 2) + pow(tuple2._2-1 - tuple2._2, 2))
    //down
    var down = sqrt(pow(tuple2._1+1 -tuple2._1, 2) + pow(tuple2._2+1 - tuple2._2, 2))
      list.addOne(right)
    list.addOne(left)
    list.addOne(up)
    list.addOne(down)
    var min = list.min
      LOGGER.info("Minimum " + min)
      return tuple2
    }
}


/*
CAN Implemetation approach
*/

class lookupdata extends Actor with ActorLogging {

  //how about if we maintain a map for each coordinate and their zone limits
  var routing_table1 = new mutable.HashMap[Tuple2[Int,Int],String]()

  var actual_pair = new mutable.HashMap[Int,Int]() //where string represents the path of node and key_value pair

  def receive = {

    case copy_routing(coordinates, path) => {
      routing_table_global.put(coordinates, path) //
    }


    case routing_table(coordinates, path) => {
      //few things need to be fix here
      // LOGGER.info("Updating the neighbours")
      routing_table1 = update(coordinates, path)
   //   LOGGER.info("routing table for nodes: " + path + "  " + routing_table1)

    }

      def update(coordinates: Tuple2[Int, Int], path: String): mutable.HashMap[Tuple2[Int, Int], String] = {
        if (store_nodes.size == 1) {
          routing_table_global.put(coordinates, path) //this is global
          routing_table1.put(coordinates, path) //this is resticted to specific nodes
        }

        else {
          //check left,right,up,down
         // LOGGER.info("Path " + path + "  " + coordinates )

          if (routing_table_global.contains((coordinates._1 - 1), coordinates._2)) {
              var paths= routing_table_global((coordinates._1 - 1), coordinates._2)
            var neighbour_path = system.actorSelection(routing_table_global((coordinates._1 - 1), coordinates._2))
            routing_table1.put(((coordinates._1 - 1), coordinates._2), paths)

          }
          if (routing_table_global.contains((coordinates._1 + 1), coordinates._2)) {
            var paths = routing_table_global((coordinates._1 + 1), coordinates._2)
            var neighbour_path = system.actorSelection(routing_table_global((coordinates._1 + 1), coordinates._2))

            routing_table1.put(((coordinates._1 + 1), coordinates._2), paths)

          }
          if (routing_table_global.contains((coordinates._1), (coordinates._2 - 1))) {
            var paths = routing_table_global((coordinates._1), coordinates._2 - 1)
            var neighbour_path = system.actorSelection(routing_table_global((coordinates._1), coordinates._2 - 1))
            routing_table1.put(((coordinates._1), coordinates._2 - 1), paths)

          }
          if (routing_table_global.contains((coordinates._1), (coordinates._2 + 1))) {
           var paths =  routing_table_global((coordinates._1), coordinates._2 + 1)
            var neighbour_path = system.actorSelection(routing_table_global((coordinates._1), coordinates._2 + 1))
            routing_table1.put(((coordinates._1), coordinates._2 + 1), paths)

          }

        }
        return routing_table1
      }


    case create_zone_bootstrap(x,y,path)=>{
      //var list1 = Tuple2[Int,Int]; //contains the zones of each node actor including the bootstrap itself
      var coordinate_list = new ListBuffer[Int];

      //  TODO: store the (key,value) pair with the path
      multiarray(x)(y) = 100 //originally we will store the key,value index here but for now store any random value
      //actual_pair.put(key,value);
      //store_pair.put(path,actual_pair)


      LOGGER.info(Integer.toString(multiarray(x)(y)))
      if(count_nodes_in_bootstrapmap==0) { //just only once
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes
//        list1.addOne(x,y)
        actual_coordinate.put(path.toString,(x,y))
        check_update_zone(x,y,store_nodes,path.toString)
        routing_table_global.put((x,y),path.toString) //

        self ! routing_table((x,y),path.toString)
      }
      if(count_nodes_in_bootstrapmap>0) {
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes
//        list1.addOne(x,y)
        actual_coordinate.put(path.toString,(x,y))
        check_update_zone(x,y,store_nodes,path.toString)
//        var testing = system.actorSelection(path)
        routing_table_global.put((x,y),path.toString) //
        //update routing table whenever a new node joins

        store_nodes.foreach(
        path=> {
          var testing = system.actorSelection(path._2)
          testing ! routing_table(actual_coordinate(path._2), path._2) //update the neighbours

        })
      }

      count_nodes_in_bootstrapmap+=1;

    }


    case create_zone(x,y,key,value) =>{
      val selection = system.actorSelection(store_nodes(0)); //this is bootstrap node //domain name basically resolves to the IP address
      //      var actual_pair = new mutable.HashMap[Int,Int]() //where string represents the path of node and key_value pair
      //      var store_pair = new mutable.HashMap[String,mutable.HashMap[Int,Int]]() //where string represents the path of node and key_value pair

      actual_pair.put(key,value)
      store_pair.put(self.path.toString,actual_pair)
      routing_table1.put((x,y),self.path.toString)
      selection!create_zone_bootstrap(x,y,self.path) //so self.path represents the newly created node

      var new_list= return_list() //returns the current active node map
      var random = scala.util.Random
      var range = random.nextInt(2)


      //  LOGGER.info("The generated random number is: " + new_list(0) + " " + range + "Coordinates "  + coordinate_zone(new_list(0)) + " " + coordinate_zone(new_list(1))  )
      //now randomly choose x,y points to join the coordinates
      //also the above returned map will help us in identifying the zone restriction


    }

      def return_list():mutable.HashMap[Int,String]={
        return store_nodes //this will return the map that bootstrap node contains of all the active current active nodes.
        //random number generator will be work as key for this map
      }
    //compare the randomly generated point p and find which zone it resides in by going over the coordinateS_zone map and then return the path of that particular node


    //Note: Routing table fixed

    case Find_data(coordinates,key) => {
      /*
      so for finding data we will first find the coordinates via the coordinates we will find the path and with the path we will retrieve the
      key value pairs
       */
//
      if(routing_table1.contains(coordinates)){ //meaning we are in the zone of that specific node with point P(coordinates)
        var path1 = routing_table1.get(coordinates).toString.replaceAll("Some","")
           var map = store_pair(path1.replaceAll("[(|)]",""))
        LOGGER.info("Found the value  " + map(key))

      }else{
        //if coordinate is not in the current zone(routing table) check for closest neighbors
          distance(coordinates) //choose the closest neighbour

      }

    }

  }
}

//
////check left,right,up,down
//if(routing_table_global.contains((coordinates._1-1),coordinates._2)){
//
//  var neighbour_path = system.actorSelection(routing_table_global((coordinates._1-1),coordinates._2))
//  routing_table1.put(coordinates,path)
//  routing_table1.put((coordinates._1-1,coordinates._2),neighbour_path.pathString)
//  neighbour_path!copy_routing(coordinates,path)
//
//}
//if(routing_table_global.contains((coordinates._1+1),coordinates._2)){
//
//  var neighbour_path = system.actorSelection(routing_table_global((coordinates._1+1),coordinates._2))
//
//  routing_table1.put(coordinates,path)
//  routing_table1.put((coordinates._1+1,coordinates._2),neighbour_path.pathString)
//
//  neighbour_path!copy_routing(coordinates,path)
//
//}
//if(routing_table_global.contains((coordinates._1),(coordinates._2-1))){
//
//  var neighbour_path = system.actorSelection(routing_table_global((coordinates._1),coordinates._2-1))
//  routing_table1.put(coordinates,path)
//  routing_table1.put((coordinates._1,coordinates._2-1),neighbour_path.pathString)
//  neighbour_path!copy_routing(coordinates,path)
//
//}
//if(routing_table_global.contains((coordinates._1),(coordinates._2+1))){
//  //          LOGGER.info("There is a neighbour down")
//  //          LOGGER.info(self.path.toString)
//
//  var neighbour_path = system.actorSelection(routing_table_global((coordinates._1),coordinates._2+1))
//  routing_table1.put(coordinates,path)
//  routing_table1.put((coordinates._1,coordinates._2+1),neighbour_path.pathString)
//  neighbour_path!copy_routing(coordinates,path)
//
//}
//
