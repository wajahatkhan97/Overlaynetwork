
import java.nio.file.Path
import java.security.MessageDigest

import akka.actor.TypedActor.{context, lookup, self}
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import lookupdata.store_nodes
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.math.BigDecimal.int2bigDecimal
import scala.language.postfixOps
import scala.util.control.Breaks.break
//https://medium.com/@TamasPolgar/what-to-do-with-5-000-000-akka-actors-381a915a0f78 "referenced for storing data in node actor(hashmap)"
object MyTesting extends App {


//  case class UserActor(title: String, ref: ActorSelection, value: String,key_actorpath:mutable.HashMap[Int,String],numbernodes:Int)
  case class Bootstrap_node_m(bs_path: String,ref:ActorRef )
  case class ServerActor(title: String)

  var all_nodes_paths = new mutable.HashMap[Int,String]()

  //case class add_node_to_ring(Key: String,ref:ActorRef ,path: ActorPath)
  val LOGGER = LoggerFactory.getLogger(classOf[Nothing])
//  val server_Data = ConfigFactory.load("servernodes.conf").getConfig("server_data")
//  val worker_Data = ConfigFactory.load("workernodes.conf").getConfig("worker_data")
  //Create Rectangle for the cartesian coordinate system with width and length of 10
  var x = 10; //width
  var y = 10; //height
  var DNS_map = new mutable.HashMap[Int,String]()
  class akka extends Actor with ActorLogging {
    var n = 0;

    def receive = {

      case Bootstrap_node_m(bs_path,ref) =>
        all_nodes_paths.put(0,bs_path)
        LOGGER.info("Bootstrap created")
    }
  }

  val system = ActorSystem("UserServerActors")

  //create node_Actors
  def MD5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes())
  }

  var counter = 0
    bootstrap() //calling function
  def bootstrap()={
    LOGGER.info("Creating Bootstrap node")
   val Bootstrap_node = system.actorOf(Props[lookupdata],"bootstrapnode")
    val Ip_address  = "127.0.0.1" //Ip address for Bootstrap nodes
    var hashValue = MD5(Ip_address)
        DNS_map.put(0,Bootstrap_node.path.toString)

    Bootstrap_node! lookupdata.create_zone_bootstrap(3,2,Bootstrap_node.path)
/*
//Bootstrap_node_m(Bootstrap_node.path.toString,Bootstrap_node.ref)
we also have to consider the possibility of node leaving the zone. So split it in such order that if any node leaves the space we can re-merge the zones.



so bootstrap node will be updated with the nodes paths each time.
so there must be some kind of action that will return existing list of nodes from bootstrap
once we return the list then we will add the new joining node to the list
 */

  }



  createserver()

  def createserver()= {
    LOGGER.info("Creating Server(Actor) Nodes")

        Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor")
//    now randomly choose x,y points to join the coordinates
    node!lookupdata.create_zone(2,2)

  }
  createserver_1()

  def createserver_1()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor"+1)
    //now randomly choose x,y points to join the coordinates
    node!lookupdata.create_zone(1,2)

  }

  createserver_2()
  def createserver_2()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor"+2)
//now randomly choose x,y points to join the coordinates
    node!lookupdata.create_zone(1,3)

  }
  //for horizontal split
  createserver_3()
  def createserver_3()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor"+3)
//now randomly choose x,y points to join the coordinates
    node!lookupdata.create_zone(1,3)

  }

  }





