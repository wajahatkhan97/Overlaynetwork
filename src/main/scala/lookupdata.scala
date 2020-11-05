import java.lang.System.Logger

import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, Props}
import MyTesting.{MD5, UserActor, akka, list_of_names_to_Assign_to_node, map, system}
import lookupdata.{Insert, add_nodering, create_fingertable, getValue, get_fingertable, store_nodenumbers, store_temporary_value, update_finger_table}

import scala.Int.{int2double, int2long}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
//Chord Implementation idea was from "Mr.Abhijeet"  .

*/
object lookupdata{
  case class getValue(Key:String)
  case class Insert(key:String , Value:String) //key value pair
  var map = new mutable.HashMap[String,String]()
  case class find_successor() ////the hashed key user request for will be looked into each of the finger_table and data will be retrieved from there
  case class find_predecessor()
  case class update_finger_table(nodesnumber:ListBuffer[Int]) //updating finger table comes in when a new node joins so that you update in the current node
  case class create_fingertable(totalnodes:Int,hashvalue:String,actorPath: ActorPath) //finger table will contains the immediate successors (atleast 2) of our current node
  var num_of_nodes_in_ring=0;
  case class add_nodering(Key:String,actorPath: ActorPath,ref: ActorRef)
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
                  println((Integer.parseInt(hashvalue)+scala.math.pow(2, counter)) % totalnodes)
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
      fingertablemap.foreach( println(_))


    }
    case update_finger_table(nodesnumber) =>{ ///nodesID's (0,1)
       fingertablemap.foreach{
        which_key=>
          val greater_equal = nodesnumber.filter(x=>x>=which_key._2) //._2 represents the identifier

            println("inside update finertable: " + greater_equal + "   " + nodesnumber.size)
       }

    }
    case add_nodering(key,actorPath,ref) =>{
      store_nodenumbers.addOne(key.toInt)
      actorspath.addOne(actorPath.toString)
      lookup_servernodes.addOne(key->actorPath.toString) //storing server path with key to access (while getting the data)
     // log.info("Node added to the ring")
     ref ! create_fingertable(2,key,actorPath) //so we are sending total number of nodes, key and the actorPath

      log.info("Update all the tables")
      actorspath.foreach{
        path=>
          var select_to_update = context.actorSelection(path)
          select_to_update!update_finger_table(store_nodenumbers)
      }
    }



    case Insert(key,value) =>{
      map.addOne(key,value)
       println(key + fingertablemap.get(key.toInt))
    //  println("Inside the hmap function:  " + self + "    " + map.get(key))
    }
    case getValue(key) =>{
      if(map.contains(key)){
      //  println("CONTAINS:" + map.get(key))
      }
      else {
     //   println("No key found")
      }
    }
    case get_fingertable()=>{
    }


  }

  }
