import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

object FileIO {
  //defino mi tipo 
  type Post = (String, String, String, String)
  // Pure function to read subscriptions from a JSON file
  def readSubscriptions(): List[String] = {
    List(
      "https://www.reddit.com/r/scala/.json?count=10",
      "https://www.reddit.com/r/learnprogramming/.json?count=10"
    )
  }

  // Pure function to download JSON feed from a URL
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
      //val date = TextProcessing.formatDateFromUTC(hora) // no anda porque no existe formatDateFromUTC

      val date = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
               .format(new java.util.Date(hora * 1000L))

      (subreditname,titulo,contentText,date)
    }
  
  }
}
