import java.lang.System.Logger

import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{MD5, UserActor, akka, list_of_names_to_Assign_to_node, map, number_nodes, server_Data, size_of_movie_list, system}
import com.typesafe.config.ConfigFactory
import lookupdata.{Find_data, actorspath, add_nodering, create_fingertable, getValue, get_fingertable, list_of_movies, store_nodenumbers, store_temporary_value, update_finger_table}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
//Chord Implementation idea was from "Mr.Abhijeet"  .

*/
object lookupdata{
  case class getValue(Key:String)
  case class Find_data(key:String , ref:ActorSelection,key_Actor:mutable.HashMap[Int,String],numbernodes:Int) //key value pair
  var map = new mutable.HashMap[String,String]()
  case class find_successor() ////the hashed key user request for will be looked into each of the finger_table and data will be retrieved from there
  case class find_predecessor()
  case class update_finger_table(nodesnumber:ListBuffer[Int]) //updating finger table comes in when a new node joins so that you update in the current node
  case class create_fingertable(totalnodes:Int,hashvalue:String,object_hashed:String,actorPath: ActorPath) //finger table will contains the immediate successors (atleast 2) of our current node
  var num_of_nodes_in_ring=0;
  case class add_nodering(Key:String,object_hashed: String,nodenumber:Int,actorPath: ActorPath,ref: ActorRef,totalnodes:Int,add_data:String)
  var actorspath = new mutable.ListBuffer[String] //List Buffer gives constant time
  var store_nodenumbers = new ListBuffer[Int]
   var store_temporary_value = 0
  case class get_fingertable()
  var list_of_movies= new ListBuffer[String]//List("movie1 computing node","movie2 computing node","movie3 computing node","movie4 computing node","movie5 computing node")
  val worker_Data = ConfigFactory.load("workernodes.conf").getConfig("worker_data")
  val size_of_movie_list = worker_Data.getConfigList("movie-values").get(0).getString("size")
  var i=0

  for(counter <- 1 to size_of_movie_list.toInt) {
    list_of_movies.addOne(worker_Data.getConfigList("movie-values").get(0).getString(counter.toString))
  }
}

class lookupdata extends Actor with ActorLogging {

  var lookup_servernodes = new mutable.HashMap[String,String]
  var counter =0
  var i =0
  var fingertablemap = new mutable.HashMap[Int,Int]()
  var fingertablemap_for_identifier_successor = new mutable.HashMap[Int,Int]()

  var store_data = new ListBuffer[String]
  var actual_store_data = new ListBuffer[String]

  def receive ={
    case create_fingertable(totalnodes,hashvalue,object_hashed,actorPath) =>{


      //so our approach is basically we will create finger table for each node
      //we also update the existing one(s) all the previous nodes created (node joining requires to keep track of the predecessor but we are
      // keeping track of all paths and with them we are retrieving finger tables and updating them
      // (this part was required in Project but we are doing it here)
        log.info("Generating finger table for Node: " + self.path)
      //generate finger table using the formula ("remember table contains the identifier and the key
          for(counter <- 0 to  totalnodes){ //instead of 3 we will consider m-1

                     store_temporary_value=((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % scala.math.pow(2,8)).toInt

                    fingertablemap.addOne(counter,((Integer.parseInt(object_hashed)+scala.math.pow(2, counter)) % scala.math.pow(2,8)).toInt)
                    fingertablemap_for_identifier_successor.addOne(((Integer.parseInt(object_hashed)+scala.math.pow(2, counter)) % scala.math.pow(2,8)).toInt,counter)//this map will add (the identifier and its successor)

          }
      }
    case update_finger_table(nodesnumber) =>{ ///nodesID's (0,1)
        fingertablemap_for_identifier_successor= update(nodesnumber,number_nodes) //update the successors
      //  println(self)
      //fingertablemap_for_identifier_successor.foreach(println(_))
    }
//most likely for project (also do the same thing for node failure)
      def update(nodesnumber:ListBuffer[Int],totalnodes:Int):mutable.HashMap[Int,Int]={
          val new_table = fingertablemap_for_identifier_successor.map{
          which_key=>
            val greater_equal = nodesnumber.filter(x=> x > (which_key._1%totalnodes)) //._1 represents the objectvalue%totalnumnodes to get the computing nodes and set successor according to it
             // println("inside update finertable: " + greater_equal + "   " + nodesnumber.size)
                  //  println(greater_equal)
                if(!greater_equal.isEmpty) {
                  which_key._1 -> greater_equal.head
                }
            else {
                  which_key._1 -> nodesnumber.head
                }
          }
        new_table
  }

    case add_nodering(key,object_hashed,nodenumber,actorPath,ref,totalnodes,add_data) =>{
      store_nodenumbers.addOne(nodenumber)
      actorspath.addOne(actorPath.toString)
      lookup_servernodes.addOne(key->actorPath.toString) //storing server path with key to access (while getting the data)
     // log.info("Node added to the ring")
      store_data.addOne(key) //so add the data (this is the data we will retrieve later on
      actual_store_data.addOne(list_of_movies(nodenumber)) //store the movie value in computing nodes
     ref ! create_fingertable(totalnodes,key,object_hashed,actorPath) //so we are sending total number of nodes, key and the actorPath
      //this approach comes in handy while looking up for data via the actors
        //println("size of actors path: " + actorspath.length)
     actorspath.foreach{
        path=>
          log.info("Updating table for node : " + path)
          var select_to_update = context.actorSelection(path)
          select_to_update!update_finger_table(store_nodenumbers)

     }
    }



    case Find_data(key,ref,keyactor_path,numbernodes) =>{


    var temp_key = key.toInt

        if(store_data.contains(key)){
          log.info("Found the value...")
          Thread.sleep(500) //wait before showing the data
          log.info("Computing node return the values of movie:  " + actual_store_data(0))
         // system.terminate()

        }
          if(i==numbernodes&&store_data.contains(key)==false) //if the temp key reaches to the max number and the data is not there that means data doesn't exist cause we searched all the nodes via table fingers
                  {
                    log.info("Data does not exist search all the nodes")
                    //  context.system.terminate()
                  }
          else {
          if( fingertablemap.contains(key.toInt)) {
            //println("Yeah node 2 have this value")
            //so here now I will get the successor and I will check if the desired data exist in that node if not then I will move on to the succesor of the current node
            /*
          say key is 0 then our successor is 1 or 0 (say its 0) then I will look for data in Successor 1 by calling its node if the data exist there then okay other wise we will move on to its successor
          key = fingertablemap.get(counter)
           */
            var temp_key1=0
            for (counter <- 0 to numbernodes) {
              temp_key = fingertablemap.get(counter).get
              if (temp_key % numbernodes == key.toInt) { //meaning go to that particular values successor
                temp_key1 = fingertablemap.get(counter).get
              } else {
                temp_key = -1
              }

            }
            if (temp_key!= -1) {
              var successor_node = fingertablemap_for_identifier_successor(temp_key1) //gives me the successor of the identifier (or computing nodes)
              //  fingertablemap_for_identifier_successor.foreach(println(_))
              if (successor_node == numbernodes) // 3 is representing total nodes (must be replaced by total number of nodes)
              {
                i = successor_node
                var store_path = context.actorSelection(actorspath(successor_node))

                log.info(store_path + "   " + "It should be node" + successor_node + " : " + actorspath(successor_node))
                //a better idea is to just create a simple list of actor paths and the retrieve the value from the index (time will be constant too when it comes to big data set)
                store_path ! Find_data(successor_node.toString, store_path, keyactor_path, numbernodes)

              } else {
                log.info("Okay so now the key is: " + temp_key1)
                i = successor_node
                var store_path = context.actorSelection(actorspath(successor_node))

                log.info(store_path + "   " + "It should be node" + successor_node + " : " + actorspath(successor_node))
                //a better idea is to just create a simple list of actor paths and the retrieve the value from the index (time will be constant too when it comes to big data set)
                store_path ! Find_data(successor_node.toString, store_path, keyactor_path, numbernodes)
              }
            }
          }
        else {

          //  log.info("nope Not in this node moving to next node")
            if (i != numbernodes) {
            var store_path = context.actorSelection(actorspath(i+1))
            store_path ! Find_data(i.toString, store_path, keyactor_path, numbernodes)
          }
          }
      }
    }


  }

  }
