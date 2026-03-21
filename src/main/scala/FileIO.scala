import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source


object FileIO {
  
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

  def downloadFeed(url: String): String = {
    val source = Source.fromURL(url)
    val content = source.mkString
    source.close()
    content
  }

  def readFile (path: String): Option[String] = {
    try {
      val source = Source.fromFile(path)
      val content = source.mkString 
      source.close()
      Some(content)

  }
    catch {
      case _: Exception => None
  } 
}

  def parserSubscriptions (content:String): Option[List[Subscription]] = {
    try {
      val json = parse(content)
      json match{
        case JArray(arr) =>
          Some(
              arr.map { elemento =>
              val name = (elemento \ "name").extract[String]
              val url = (elemento \ "url").extract[String]
              (name,url) 
                      }
              )
        case _ => None
                }
         }
    catch {
      case _: Exception => None
    }
  }

  def loadSubscriptions(path: String): Option[List[Subscription]] = {
    readFile(path) match {
      case Some(content) => parserSubscriptions(content)
      case None          => None
    }
  } 


}

