import org.jsoup.Jsoup

import java.io.{File, FileReader, FileWriter}
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import collection.JavaConverters.*

class Crawler {

  val constWiki="https://en.wikipedia.org"
  var PagesWithLinks=new ListBuffer[(String,ListBuffer[String] )]()

  def Crawl(n:Int):ListBuffer[(String,ListBuffer[String] )] = {
    var pageStr="/wiki/Rzesz%C3%B3w";
    var PagesWithRefs=new ListBuffer[(String,ListBuffer[String])]()
    for(i<-0 to n-1)
      {
        var page=Jsoup.connect(constWiki+pageStr).get()
        val links=page.select("a[href]")
        PagesWithRefs+=((pageStr,ListBuffer.empty[String]))
        for (link<-links.asScala) {
          val href = link.attr("href")
          if((href.startsWith("/wiki/"))&&(!href.contains("Wikipedia:"))&&(!(href.contains("File:")))&&(!(href.contains("Template:")))&&(!(href.contains("Help:")))&&(!(href.contains("Special:"))))
            PagesWithRefs(i)._2+=href
        }
        pageStr=(PagesWithRefs(i)._2)(Random.nextInt(PagesWithRefs(i)._2.size))
        println(i)
      }
      PagesWithLinks=PagesWithRefs
    return PagesWithRefs
  }

  def PageRank( iterations:Int)={
    var pages=ListBuffer[String]()
    val n=PagesWithLinks.size
    for (page<-PagesWithLinks)
      {
        pages+=page._1
      }
    var relationMatrix = Array.ofDim[Double](n,n)
    for (i<-0 to n-1) {
      for (j <- 0 to n - 1) {
        if(PagesWithLinks(j)._2.contains(pages(i)))
            relationMatrix(i)(j)=1.0
        else
          relationMatrix(i)(j)=0.0
      }
    }
    var NodeRanks=Array.ofDim[Int](n)
    for (i<-0 to n-1)
      for (j<-0 to n-1)
      {
        {
          if (relationMatrix(i)(j) > 0)
            NodeRanks(j) += 1
        }
      }
    for (i<-0 to n-1) {
      for (j <- 0 to n - 1) {
          relationMatrix(i)(j)=relationMatrix(i)(j)/NodeRanks(j)
      }
    }
    var PRs=Array.fill(n)(1.0/n)
    for (i<-0 to iterations-1) {
      var PRsOld=PRs.clone()
      for (i <- 0 to n - 1) {
        var sum = 0.0
        for (j <- 0 to n - 1) {
          sum += (relationMatrix(i)(j) * PRsOld(j))
        }
        PRs(i) = sum
      }
      var PagesWithPRs=new ListBuffer[(String,Double)]()
      for (i<-0 to n-1)
        {
          PagesWithPRs+=((pages(i),PRs(i)))
        }
        var sortedPagesWithPRs=PagesWithPRs.sortWith(_._2 > _._2)
        SavePRsToCSv("Pagerank",sortedPagesWithPRs)
    }
  }
  def LinkAnalysis(link:String)={
    var pagesWithReferences=PagesWithLinks
    var pagesWithLinkAndCounter=new ListBuffer[(String,Int)]
    var occurenciesOfLink=0.0
    for (page<-pagesWithReferences) {
      if (page._2.contains(link)) {
        pagesWithLinkAndCounter += ((page._1, page._2.count(x => x == link)))
        occurenciesOfLink += page._2.count(x => x == link)
      }
    }
      val average=occurenciesOfLink/pagesWithReferences.length
    println("Analysis of link: "+link)
    println("Occurencies of link: "+occurenciesOfLink)
    println("Average: "+average)
    for (page<-pagesWithLinkAndCounter)
      {
        println(page._1+", "+page._2)
      }
  }
  def SaveCrawledToCSV(filename:String):Unit={
    val fileWriter = new FileWriter(new File("csv/"+filename+".csv"))
    for(page<-PagesWithLinks) {
      fileWriter.write(page._1+", ")
      for(link<-page._2)
        {
          fileWriter.write(link+", ")
        }
        fileWriter.write("\n")
    }
    fileWriter.close()
  }
  def ReadCrawledFromCSV(filename:String)={
    val bufferedSource = io.Source.fromFile("csv/"+filename)
    var PagesWithRefs=new ListBuffer[(String,ListBuffer[String])]()
    var i =0
    for (line<-bufferedSource.getLines())
      {
        val vals=line.split(", ").map(_.trim)
        PagesWithRefs+=((vals(0),new ListBuffer[String]))
        for(j<-1 to vals.length-1)
          {
            PagesWithRefs(i)._2+=vals(j)
          }
        i+=1
      }
    PagesWithLinks=PagesWithRefs
  }

  def SavePRsToCSv(filename:String,pagesWithPRs:ListBuffer[(String,Double)] )={
    val fileWriter = new FileWriter(new File("csv/"+filename+".csv"))
    for(page<-pagesWithPRs) {
      fileWriter.write(page._1+", ")
      fileWriter.write(page._2+", "+"\n")
    }
    fileWriter.close()
  }
}
