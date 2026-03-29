object Main {
  def main(args: Array[String]): Unit = {

    val header = s"Reddit Post Parser\n${"=" * 40}"

    // 1. Leer name + url
    val subscriptions: List[FileIO.Subscription] = FileIO.loadSubscriptions("subscriptions.json").getOrElse(List())

    // 2. Descargar posts
    val allPosts = subscriptions.map { case (name, url) =>
      println(s"Fetching posts from: $url")

      val posts = FileIO.downloadFeed(url)
        .map(FileIO.postList)
        .getOrElse(List())

      (name, url, posts)
    }

    // 3. Output completo
    val output = allPosts.map { case (name, url, posts) =>

      val totalScore = FileIO.totalScore(posts)

      // palabras más frecuentes
      val allText = posts.map(_._3).mkString(" ")
      val palabras = FileIO.countwords(allText)
        .toList
        .sortBy(-_._2)
        .take(5)

      // top 5 posts
      val top5 = posts
        .sortBy { case (_, _, _, _, score) => -score }
        .take(5)

      s"""
Subreddit: $name
URL: $url
Total Score: $totalScore
Cantidad de posts: ${posts.length}

Top palabras:
${palabras.map { case (p, c) => s"$p: $c" }.mkString("\n")}

Top 5 posts:
${top5.map { case (_, titulo, _, fecha, score) =>
  s"$titulo | $fecha | Score: $score"
}.mkString("\n")}
"""
    }.mkString("\n============================\n")

    println(header)
    println(output)
  }
}