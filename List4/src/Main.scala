import org.jsoup.Jsoup

import collection.JavaConverters.*
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
object Main {

  def main(args: Array[String]) = {
    val crawler = new Crawler()

    crawler.Crawl(10)
    crawler.PageRank(10)
    crawler.SaveCrawledToCSV("crawled")
    crawler.ReadCrawledFromCSV("crawled.csv")
    crawler.LinkAnalysis("/wiki/Rzesz%C3%B3w")
    var option = ""
    while (option != "q") {
      println("Select option (c,pr,la)")
      option = readLine()
      option match {
        case "q" => //do nothing and end
          println("Quit!")
        case "c" =>
          crawler.Crawl(10)
          crawler.SaveCrawledToCSV("crawled")
        case "pr" =>
          crawler.ReadCrawledFromCSV("crawled.csv")
          crawler.PageRank(10)
        case "la" =>
          crawler.ReadCrawledFromCSV("crawled.csv")
          crawler.LinkAnalysis("/wiki/Rzesz%C3%B3w")
      }
    }
  }
}
