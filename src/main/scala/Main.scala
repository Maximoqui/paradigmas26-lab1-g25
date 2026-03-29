object Main {
  def main(args: Array[String]): Unit = {

    val header = s"Reddit Post Parser\n${"=" * 40}"

    // 1. Leer URLs de subscriptions.json
    val subscriptions: List[String] = FileIO.readSubscriptions()

    // 2. Descargar y parsear los posts de cada URL
    val allPosts: List[(String, List[FileIO.Post])] = subscriptions.map { url =>
      println(s"Fetching posts from: $url")

      val posts: List[FileIO.Post] = FileIO.downloadFeed(url)
        .map(FileIO.postList)
        .getOrElse(List())

      (url, posts)
    }

    // 3. Generar el output final
    val output = allPosts.map { case (url, posts) =>

      // calcular score total
      val totalScore = FileIO.totalScore(posts)

      s"""
URL: $url
Total Score: $totalScore
Cantidad de posts: ${posts.length}
"""
    }
    .mkString("\n")

    // 4. Imprimir resultado
    println(header)
    println(output)
  }
}