object Main {
  def main(args: Array[String]): Unit = {
    val header = s"Reddit Post Parser\n${"=" * 40}"

    val subscriptions: List[String] = FileIO.readSubscriptions()

    val allPosts: List[(String, String)] = subscriptions.map { url =>
      println(s"Fetching posts from: $url")
      // downloadFeed devuelve un Option. Usamos getOrElse para convertirlo en post
      // si hay éxito extrae el texto si hay error devuelve un String vacío ("").
    val posts = FileIO.downloadFeed(url)
      .getOrElse("")
    (url, posts)
    }

    val output = allPosts
      .map { case (url, posts) => Formatters.formatSubscription(url, posts) }
      .mkString("\n")

    println(output)
  }
}
