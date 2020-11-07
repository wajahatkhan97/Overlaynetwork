import java.lang.System.Logger

import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, Props}
import MyTesting.{MD5, UserActor, akka, list_of_names_to_Assign_to_node, map, system}
import lookupdata.{Find_data, add_nodering, create_fingertable, getValue, get_fingertable, store_nodenumbers, store_temporary_value, update_finger_table}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
//Chord Implementation idea was from "Mr.Abhijeet"  .

*/
object lookupdata{
  case class getValue(Key:String)
  case class Find_data(key:String , ref:ActorSelection,key_Actor:mutable.HashMap[Int,String]) //key value pair
  var map = new mutable.HashMap[String,String]()
  case class find_successor() ////the hashed key user request for will be looked into each of the finger_table and data will be retrieved from there
  case class find_predecessor()
  case class update_finger_table(nodesnumber:ListBuffer[Int]) //updating finger table comes in when a new node joins so that you update in the current node
  case class create_fingertable(totalnodes:Int,hashvalue:String,actorPath: ActorPath) //finger table will contains the immediate successors (atleast 2) of our current node
  var num_of_nodes_in_ring=0;
  case class add_nodering(Key:String,actorPath: ActorPath,ref: ActorRef,totalnodes:Int,add_data:String)

  var store_nodenumbers = new ListBuffer[Int]
   var store_temporary_value = 0
  case class get_fingertable()

}

class lookupdata extends Actor with ActorLogging {
  var actorspath = new mutable.ListBuffer[String] //List Buffer gives constant time
  var lookup_servernodes = new mutable.HashMap[String,String]
  var counter =0
  var i =0
  var fingertablemap = new mutable.HashMap[Int,Int]()
  var store_data = new ListBuffer[String]
  def receive ={
    case create_fingertable(totalnodes,hashvalue,actorPath) =>{


      //so our approach is basically we will create finger table for each node
      //we also update the existing one(s) all the previous nodes created (node joining requires to keep track of the predecessor but we are
      // keeping track of all paths and with them we are retrieving finger tables and updating them
      // (this part was required in Project but we are doing it here)
        println("Generating finger table for Node: " + self.path)
      //generate finger table using the formula ("remember table contains the identifier and the key
          for(counter <- 0 to  3){ //instead of 3 we will consider m-1
            //counter represent the keys (or ID for our nodes) and the formula (value) is the identifier scala.math.pow(2, totalnodes))
            //so the result of the formula will give us successor
                //fingertablemap+=(counter->((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % totalnodes).toInt) //6
               //   println("Server Node is: " + (Integer.parseInt(hashvalue) + "  The key is: " + counter + " And the value is: "+ ((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % scala.math.pow(2, totalnodes)).toInt))
                    store_temporary_value=((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % totalnodes).toInt
             //     println((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % totalnodes)
               //   for( i<- 0 to store_nodenumbers.size){
                    fingertablemap.addOne(counter,((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % totalnodes).toInt)
              //    val successor_for_key=  (store_nodenumbers.foreach(x => x >= store_temporary_value))
//             // store_nodenumbers.foreach()
//              for(e<- store_nodenumbers) {
//                      if(e>=store_temporary_value){
//                      val successor_for_key= e
//                       // println("Server Node is: " + (Integer.parseInt(hashvalue) + "  The key is: " + counter +  " And the calculated Identifier is: "+ store_temporary_value + "Successor for the Node "+successor_for_key) )
//                        fingertablemap+=(counter->e)
//                      }
//                      else{
//                     //   store_temporary_value = store_temporary_value%2; //hardcoding the total number of nodes in case it goes over our limit
//                        //(this is the case where we get the identifier which is out of our index. So, 4 is basically our Numoftotalnodes)
//                        for(e<-store_nodenumbers) {
//                          if(e>=store_temporary_value){
//                            val successor_for_key= e
//                         //   println("2Server Node is: " + (Integer.parseInt(hashvalue) + "  2The key is: " + counter +  " 2 the calculated Identifier is: " + store_temporary_value + "  2Successor for the Node "+successor_for_key) )
//                            fingertablemap+=(counter->e)
//                          }
//                        }
//                      }
//                    }

          }
   //   println(self.path)
     // fingertablemap.foreach( println(_))


    }
    case update_finger_table(nodesnumber) =>{ ///nodesID's (0,1)
      println(self)
        //fingertablemap= update(nodesnumber) //no need to update

      fingertablemap.foreach( println(_))
    }

//      def update(nodesnumber:ListBuffer[Int]):mutable.HashMap[Int,Int]={
//          val new_table = fingertablemap.map{
//          which_key=>
//            val greater_equal = nodesnumber.filter(x=> x > which_key._2) //._2 represents the identifier
//
//             // println("inside update finertable: " + greater_equal + "   " + nodesnumber.size)
//                if(!greater_equal.isEmpty) {
//                  which_key._1 -> greater_equal.head
//                }
//            else {
//                  println("else condition: " + nodesnumber.head )
//                  which_key._1 -> nodesnumber.head
//                }
//          }
//        new_table
//  }

    case add_nodering(key,actorPath,ref,totalnodes,add_data) =>{
      store_nodenumbers.addOne(key.toInt)
      actorspath.addOne(actorPath.toString)
      lookup_servernodes.addOne(key->actorPath.toString) //storing server path with key to access (while getting the data)
     // log.info("Node added to the ring")
      store_data.addOne(add_data) //so add the data (this is the data we will retrieve later on
     ref ! create_fingertable(totalnodes,key,actorPath) //so we are sending total number of nodes, key and the actorPath
      //this approach comes in handy while looking up for data via the actors
      log.info("Update all the tables")
     actorspath.foreach{
        path=>
          var select_to_update = context.actorSelection(path)
          select_to_update!update_finger_table(store_nodenumbers)
      }
    }



    case Find_data(key,ref,keyactor_path) =>{

     //  println(key + fingertablemap.get(key.toInt))
    //  println("Inside the hmap function:  " + self + "    " + map.get(key))
var temp_key = key.toInt
     // for(counter<-0 to 3){
      //this is working fine but need a condition where if it searched all the nodes it should stop.
        if(store_data.contains("Krunal")){
          println("Computing node: " + self + "  Contains the data") //he is the computing node
          system.terminate()
        }
          if(temp_key==3&&store_data.contains("Krunal")==false) //if the temp key reaches to the max number and the data is not there that means data doesn't exist cause we searched all the nodes via table fingers
                  {
                    println("Data does not exist search all the nodes")
                  }
          else {
          if( fingertablemap.contains(key.toInt)){
          println("Yeah node 2 have this value")
        //so here now I will get the successor and I will check if the desired data exist in that node if not then I will move on to the succesor of the current node
          /*
          say key is 0 then our successor is 1 or 0 (say its 0) then I will look for data in Successor 1 by calling its node if the data exist there then okay other wise we will move on to its successor
          key = fingertablemap.get(counter)
           */
           temp_key = fingertablemap.get(key.toInt).get
            if(temp_key==3) // 3 is representing total nodes
          {
            var store_path = context.actorSelection(keyactor_path.get(temp_key).get)
            println(store_path + "   " + "It should be node 1: " + keyactor_path.get(temp_key).get)
            //a better idea is to just create a simple list of actor paths and the retrieve the value from the index (time will be constant too when it comes to big data set)
            store_path!Find_data(temp_key.toString,store_path,keyactor_path)

          }else {
              println("Okay so now the key is: " + temp_key)
              var store_path = context.actorSelection(keyactor_path.get(temp_key + 1).get)
              println(store_path + "   " + "It should be node 1: " + keyactor_path.get(temp_key + 1).get)
              //a better idea is to just create a simple list of actor paths and the retrieve the value from the index (time will be constant too when it comes to big data set)
              store_path ! Find_data(temp_key.toString, store_path, keyactor_path)
            }
          }
        else {
          println("nope")
        }
      }
    }
    case getValue(key) =>{
      if(map.contains(key)){
      //  println("CONTAINS:" + map.get(key))
      }
      else {
     //   println("No key found")
      }
    }


  }

  }
