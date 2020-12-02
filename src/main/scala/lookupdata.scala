
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{LOGGER, MD5, akka, server_Data, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, actual_coordinate, actual_coordinate_zone, check_update_zone, coordinate_list, coordinate_zone, count_nodes_in_bootstrapmap, create_zone, create_zone_bootstrap, find_zone, list, multiarray, predefined_coordinate_zone, print_Space, store_nodes, update_zones}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._
import scala.util.control.Breaks.break

/*
/*
CAN Implemetation approach
*/*/
object lookupdata {

  case class Find_data(key: String, ref: ActorSelection, key_Actor: mutable.HashMap[Int, String], numbernodes: Int) //key value pair
  case class create_zone_bootstrap(x: Int, y: Int, path: ActorPath)

  case class create_zone(x: Int, y: Int)

  var x = 4; //width
  var y = 4; //height
  var count_nodes_in_bootstrapmap = 0
  var coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()
  var actual_coordinate_zone = new mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]()

  var predefined_coordinate_zone = new mutable.HashMap[Int, ListBuffer[Tuple2[Int, Int]]]()


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
  //(x1,y1)(x2,y2)
  def find_zone(x: Int, y: Int) = {

  }

  def check_update_zone(x:Int,y:Int,store_nodes:mutable.HashMap[Int, String],actual_Cordinates:mutable.HashMap[String, ListBuffer[Tuple2[Int, Int]]]) = {
    /*
      X - coordinate
      Y - coordinate
    //TODO: STORE KEY VALUE AT THE INDEX
     */
    var counting=0;
    var x_coordinate = false
    var y_coordinate = false
    var lower_bound_X = 0;
    var lower_bound_y=0
    var upper_bound_x=0
    var upper_bound_y=0

    if(store_nodes.size==1){ //means we only have 1 node here
        for(count <- 0 until predefined_coordinate_zone.size){
          predefined_coordinate_zone(count).foreach(
            value =>
              {
                //Is it in this node
                if(counting==0) {
                  if (value._1 <= x && x < value._2) {

                    LOGGER.info("Pre-defined zones" + value)
                    x_coordinate = true
                  }
                }else{
                  if(value._1 <= y  && y < value._2){

                    LOGGER.info("Pre-defined zones" + value)
                    y_coordinate = true
                  }
                }

                if(x_coordinate&&y_coordinate){
                  //update the zone for this
                      //first step check the number of nodes in this zone
                      // second
                   actual_Cordinates.put(store_nodes(count), predefined_coordinate_zone(count))
                  x_coordinate = false
                  y_coordinate = false
                }
                counting +=1
              }
          )

        }
      }else{
      for(count <- 0 until predefined_coordinate_zone.size){
        var x_coordinate = false
        var y_coordinate = false
        predefined_coordinate_zone(count).foreach(
          value =>
          {
            //Is it in this node
            if(counting%2==0) { //even sequence
              if (value._1 <= x && x <= value._2) {

                LOGGER.info("Pre-defined zones" + value)
                x_coordinate = true
              }
            }else{
              if(value._1 <= y  && y <= value._2){
                y_coordinate = true
              }
            }
            if(x_coordinate&&y_coordinate){
              var list_trying = new ListBuffer[Tuple2[Int, Int]]
              var new_count = 0
              for (i <- 0 to 1) {
                for (j <- 0 to 1) {
                    if(multiarray(i)(j)!=0){
                      list_trying.addOne(i,j) //add the index of the available node here in this particular zone
                      LOGGER.info("Pre-defined zones" + list_trying)

                    }
                  }
              }
              //now is the part where you update the zones compare list(0)(1)

              x_coordinate = false
              y_coordinate = false
            }
            counting +=1
          }
        )
      }
    }
  }



  def update_zones(x:Int) = {
    var pre_defined = new ListBuffer[Tuple2[Int, Int]]; //contains the zones of each node actor including the bootstrap itself
      if(x==0){
        pre_defined.addOne(0,1) //(x1,x2)
        pre_defined.addOne(0,1) //(y1,y2)
        predefined_coordinate_zone.put(x,pre_defined)
      }

    if(x==1){
      pre_defined.addOne(2,3) //(x1,x2)
      pre_defined.addOne(0,1) //(y1,y2)
      predefined_coordinate_zone.put(x,pre_defined)

    }

    if(x==2){
      pre_defined.addOne(0,1) //(x1,x2)
      pre_defined.addOne(2,3) //(y1,y2)
      predefined_coordinate_zone.put(x,pre_defined)

    }

    if(x==3){
      pre_defined.addOne(2,3) //(x1,x2)
      pre_defined.addOne(2,3) //(y1,y2)
      predefined_coordinate_zone.put(x,pre_defined)

    }
  }
}


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
      for(x <- 0 to multiarray.length) {
        update_zones(x)
      }

      if(count_nodes_in_bootstrapmap==0) { //just only once
        list.addOne(0, 1) //4x4 x
        list.addOne(0, 4)//y
        coordinate_list.addOne(x)
        coordinate_list.addOne(y)

        actual_coordinate.put(path.toString,coordinate_list)
        actual_coordinate_zone.put(path.toString, list) //this list will be the one to get updated
        store_nodes.put(count_nodes_in_bootstrapmap, path.toString()) //store the nodes
        check_update_zone(x,y,store_nodes,actual_coordinate_zone) //returns the zone

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
