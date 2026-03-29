import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source

object FileIO {
  
  // Definición de tipos
  type Post = (String, String, String, String, Int)
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

  def downloadFeed(url: String): Option[String] = {
    try {
      val source = Source.fromURL(url) 
      val content = source.mkString
      source.close()
      Some(content)
    } catch {
      case _: Exception => None
    }
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
     
      val score = (datos \"data"\ "score").extract[Int]

      (subreditname,titulo,contentText,date,score)
    }
    .filter { case (_, titulo, contentText,_,_) =>
      contentText.trim.nonEmpty && titulo.trim.nonEmpty
    }
  }  
  def countwords(subText: String): Map[String, Int] = {
    val stopwords: Set[String] = Set(
      "the", "about", "above", "after", "again", "against", "all", "am", "an",
      "and", "any", "are", "aren't", "as", "at", "be", "because", "been",
      "before", "being", "below", "between", "both", "but", "by", "can't",
      "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't",
      "doing", "don't", "down", "during", "each", "few", "for", "from", "further",
      "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd",
      "he'll", "he's", "her", "here", "here's", "hers", "herself", "him",
      "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if",
      "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me",
      "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off",
      "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves",
      "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's",
      "should", "shouldn't", "so", "some", "such", "than", "that", "that's",
      "the", "their", "theirs", "them", "themselves", "then", "there", "there's",
      "these", "they", "they'd", "they'll", "re", "they've", "this", "those",
      "through", "to", "too", "under", "until", "up", "very", "was", "wasn't",
      "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what",
      "what's", "when", "when's", "where", "where's", "which", "while", "who",
      "who's", "whom", "why", "why's", "with", "won't", "would",
      "wouldn't", "you", "you'd", "you'll", "you're", "you_ve", "your", "yours",
      "yourself", "yourselves"
    )

    subText
      .split("\\W+")
      .filter { p =>
        p.nonEmpty &&
        p.head.isUpper &&
        !stopwords.contains(p.toLowerCase)
      }
      .groupBy(identity)               
      .map { case (palabra, lista) => (palabra, lista.length) }
  }

}