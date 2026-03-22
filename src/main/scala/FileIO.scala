import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source

object FileIO {
  
  // Definición de tipos
  type Post = (String, String, String, String)
  type Subscription = (String, String)
  implicit val formats: Formats = DefaultFormats 

  def readSubscriptions(): List[String] = {
    loadSubscriptions("subscriptions.json") match {
      case Some(subscriptions) =>
        subscriptions.map { case (_, url) => url }
      case None =>
        List()
    }
  }

  def readFile(path: String): Option[String] = {
    try {
      val source = Source.fromFile(path)
      val content = source.mkString 
      source.close()
      Some(content)
    } catch {
      case _: Exception => None
    } 
  }

  def parserSubscriptions(content: String): Option[List[Subscription]] = {
    try {
      val json = parse(content)
      json match {
        case JArray(arr) =>
          Some(arr.map { elemento =>
            val name = (elemento \ "name").extract[String]
            val url = (elemento \ "url").extract[String]
            (name, url) 
          })
        case _ => None
      }
    } catch {
      case _: Exception => None
    }
  }

  def loadSubscriptions(path: String): Option[List[Subscription]] = {
    readFile(path) match {
      case Some(content) => parserSubscriptions(content)
      case None          => None
    }
  } 

  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    val content = source.mkString
    source.close()
    content
  }

 def postList(posts: String): List[Post] = {
    // esto es para el .extract
    implicit val formats: DefaultFormats.type = DefaultFormats 
    //transforma el string q descarge de downloadFeed en una estructura tipo arbol para poder "navegar" por el con \
    val jsonposts = parse(posts)
    // convierte la rama en una lista para poder usar el map 
    val datos_tupla = (jsonposts \ "data" \ "children").children

    datos_tupla.map { datos =>

      val subreditname = (datos \"data"\ "subreddit").extract[String]
      val titulo = (datos \"data"\ "title" ).extract[String]
      val contentText = (datos \"data"\ "selftext").extract[String]

      val hora = (datos \"data"\ "created_utc").extract[Double].toLong
      
      val date = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
               .format(new java.util.Date(hora * 1000L))

      (subreditname,titulo,contentText,date)
    }
    .filter { case (subreditname, titulo, contentText, date) =>
      contentText.trim.nonEmpty && titulo.trim.nonEmpty
    }
  }
}