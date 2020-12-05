import java.nio.file.Path
import java.security.MessageDigest

import akka.actor.TypedActor.{context, lookup, self}
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{Actor, ActorLogging, ActorPath, ActorRef, ActorSelection, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import lookupdata.Find_data
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.math.BigDecimal.int2bigDecimal
import scala.language.postfixOps
import scala.util.control.Breaks.break
//https://medium.com/@TamasPolgar/what-to-do-with-5-000-000-akka-actors-381a915a0f78 "referenced for storing data in node actor(hashmap)"
class MyTesting  {


  //  case class UserActor(title: String, ref: ActorSelection, value: String,key_actorpath:mutable.HashMap[Int,String],numbernodes:Int)
  case class Bootstrap_node_m(bs_path: String,ref:ActorRef )
  case class ServerActor(title: String)

  var all_nodes_paths = new mutable.HashMap[Int,String]()
  var list = new ListBuffer[Tuple2[Int,Int]]
  var movie_titles =  List[String] ("movie1","movie2","movie3")
  var movie_objects =  List[String] ("12","13","14")
  var store_paths = new ListBuffer[String]
  var list_paths = new ListBuffer[String]

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
 // bootstrap() //calling function
  def bootstrap()={
    LOGGER.info("Creating Bootstrap node")
    val Bootstrap_node = system.actorOf(Props[lookupdata],"bootstrapnode")
    val Ip_address  = "127.0.0.1" //Ip address for Bootstrap nodes
    var hashValue = MD5(Ip_address)
    DNS_map.put(0,Bootstrap_node.path.toString)
    var x =3
    var y =2
    if(!list.contains((x,y))) {
      list.addOne((x,y))
      list_paths.addOne(Bootstrap_node.path.toString)

      Bootstrap_node! lookupdata.create_zone_bootstrap(3,2,Bootstrap_node.path)
    }else{
      LOGGER.info("Node Already exist")
    }
    /*
    //Bootstrap_node_m(Bootstrap_node.path.toString,Bootstrap_node.ref)
    we also have to consider the possibility of node leaving the zone. So split it in such order that if any node leaves the space we can re-merge the zones.
    so bootstrap node will be updated with the nodes paths each time.
    so there must be some kind of action that will return existing list of nodes from bootstrap
    once we return the list then we will add the new joining node to the list
     */

  }



  //createserver()

  def createserver()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor")
    //    now randomly choose x,y points to join the coordinates
    var x =2
    var y =2
    if(!list.contains((x,y))) {
      list.addOne((x, y))
      list_paths.addOne(node.path.toString)
      var hashValue_movietitle = MD5(movie_titles(0))
      var hashValue_movieobjects = MD5(movie_objects(0))

      node ! lookupdata.create_zone(2, 2,hashValue_movietitle(0),hashValue_movieobjects(0))
    }else{
      LOGGER.info("Node Already exist")
    }
  }
  //createserver_1()

  def createserver_1()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor"+1)
    //now randomly choose x,y points to join the coordinates
    var x =1
    var y =2
    if(!list.contains((x,y))) {
      list.addOne((x, y))
      var hashValue_movietitle = MD5(movie_titles(1))
      var hashValue_movieobjects = MD5(movie_objects(1))
      list_paths.addOne(node.path.toString)

      node ! lookupdata.create_zone(1, 2,hashValue_movietitle(0),hashValue_movieobjects(0))
    }else{
      LOGGER.info("Node Already exist")
    }
  }

  //createserver_2()
  def createserver_2()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    // Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor"+2)
    //now randomly choose x,y points to join the coordinates
    var x =1
    var y =3
    if(!list.contains((x,y))) {
      list.addOne((x, y))
      var hashValue_movietitle = MD5(movie_titles(2))
      var hashValue_movieobjects = MD5(movie_objects(2))
      list_paths.addOne(node.path.toString)

      node ! lookupdata.create_zone(1, 3,hashValue_movietitle(0),hashValue_movieobjects(0))
    }else{
      LOGGER.info("Node Already exist")
    }
  }
  //for horizontal split
  //createserver_3()
  def createserver_3()= {
    LOGGER.info("Creating Server(Actor) Nodes")

    // Thread.sleep(1500) //1.5 sec delay
    implicit val timeout = Timeout(5 seconds)

    val node = system.actorOf(Props[lookupdata],"nodeactor"+3)
    //now randomly choose x,y points to join the coordinates
    var x =2
    var y =3
    if(!list.contains((x,y))) {
      list.addOne((x, y))
      list_paths.addOne(node.path.toString)
      var hashValue_movietitle = MD5(movie_titles(2))

      node ! lookupdata.create_zone(2, 3,hashValue_movietitle(0),0)
    }else{
      LOGGER.info("Node Already exist")
    }
  }
 // usernode()
  def usernode(): Unit ={

    val node = system.actorSelection(list_paths(3))
    var hashValue_movietitle = MD5(movie_titles(2))
    node!Find_data((1,3),hashValue_movietitle(0))
  }

}

